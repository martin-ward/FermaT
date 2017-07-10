#############################################################################
## FermaT Transformation System
## Copyright (C) 2001 Software Migrations Limited.
## Email: martin@gkc.org.uk
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
#############################################################################
#
# Static Single Assignment construction
#
# See "The Static Single Assignment Form and its Computation"
# G. Bilardi and K. Pingali (SSA-PDG/pingali-ssa.ps.gz)
#

package SSA;
require 5.000;
require Exporter;
use Carp;

@ISA = qw(Exporter);
@EXPORT = qw(build_adt phi_placement rename_vars struct_preprocess byte_preprocess
	     register_preprocess calculate_proc_summaries calculate_SSA);

use strict;
use warnings;
use Blocks;
use Set::IntRange;

no warnings "recursion";

sub visit_incr($$$$$);
sub rename_vars_sub($$$);
sub expand($$);
sub byte_refs($);
sub reg_field_refs($$);
sub dfs_list($$$$);


# Additional keys for the hashes in the @$bb array:

# bndry		Record if this is a boundary node
# next_bndry	Link to the nearest ancestral boundary node
# L		List of up-edges starting at this node,
#		OR (for bndry nodes) EDF(w), ordered so that a node
#		appears *after* its ancestors



# Build the Augmented Dominator Tree structure
# Inputs: $bb: array ref of nodes in the control flow graph
#	  $start: the start node
#	  $beta: a real number parameter
# Outputs: Sets the bndry, next_bndry and L keys in the nodes
# Uses temp keys b, t, a and z
#

sub build_adt($$$) {
  my ($bb, $start, $beta) = @_;

  # Calculate the dominator tree:
  calculate_idom_fast2($bb, $start, "succs", "preds", "idom");

  # We want to walk the dominator tree top-down and bottom-up
  # so we do a depth first scan of the reverse of idom:
  calculate_reverse($bb, "idom", "rev_idom");
  my $top_down = depth_first_list($start, "rev_idom", "dfs_num", "dfsp");
  my $bottom_up = [reverse(@$top_down)];
  foreach my $x (@$bb) {
    $x->{b} = 0;
    $x->{t} = 0;
  }
  # Loop over all the up-edges
  # (these are CFG edges which are not in rev_idom
  # ie edges u->v where u is not idom(v))
  foreach my $u (@$bb) {
    foreach my $v (@{$u->{succs}}) {
      next if ($u->{num} == $v->{idom}->{num});
      $u->{b}++;
      $v->{idom}->{t}++;
    }
  }
  # Determine bndry nodes:
  foreach my $x (@$bottom_up) {
    # Compute the output size when $x is queried
    # and the tentative zone size
    $x->{a} = $x->{b} - $x->{t};
    $x->{z} = 1;
    my @children = @{$x->{rev_idom}};
    foreach my $y (@children) {
      $x->{a} += $y->{a};
      $x->{z} += $y->{z};
    }
    # All leaf nodes are boundary nodes,
    # otherwise make the zone as big as possible, but not bigger
    # than alpha * output_size(v) + 1
    if (!@children || ($x->{z} > $beta * $x->{a} + 1)) {
      # Begin a new zone
      $x->{bndry} = 1;
      $x->{z} = 1;
    } else {
      # Put $x into the same zone as its children:
      $x->{bndry} = 0;
    }
  }
  # Chain each node to the first bndry node that is an ancestor:
  foreach my $x (@$top_down) {
    if ($x->{num} == $start->{num}) {
      $x->{next_bndry} = -1e99; # -infinity
    } elsif ($x->{idom}->{bndry}) {
      $x->{next_bndry} = $x->{idom};
    } else {
      $x->{next_bndry} = $x->{idom}->{next_bndry};
    }
  }
  # Build the lists L[x]
  # Loop over all the up-edges
  foreach my $u (@$bb) {
    foreach my $v (@{$u->{succs}}) {
      next if ($u->{num} == $v->{idom}->{num});
      my $w = $u;
      # while idom(v) properly dominates w do...
      while ($v->{idom}->{dfs_num} < $w->{dfs_num}) {
	push(@{$w->{L}}, $v);
	$w = $w->{next_bndry};
      }
    }
  }
}


# Compute the set of nodes where phi functions are needed.
# This assumes that build_adt has already been called.
# Returns the set of nodes as a hash ref whose keys
# are the node numbers.

sub phi_placement($$$$) {
  my ($bb, $start, $S, $var) = @_;
  # Priority queue: an array of lists, one for each level in the tree:
  my $PQ = [];
  # Set of join nodes (hash ref of node numbers):
  my $J = {};
  # Compute level numbers and insert nodes in S into the PQ list:
  calculate_level($bb, $start, "rev_idom", "level") unless defined($start->{level});
  foreach my $n (@$S) {
    push(@{$$PQ[$n->{level}]}, $n);
  }
  # Mark all nodes in S:
  foreach my $n (@$S) {
    $n->{mark} = 1;
  }
  my $level = $#$PQ;
  while ($level >= 0) {
    while ($$PQ[$level] && @{$$PQ[$level]}) {
      query_incr(pop(@{$$PQ[$level]}), $PQ, $J, $var);
    }
    $level--;
  }
  # Clear all marks:
  foreach my $n (@$bb) {
    delete $n->{mark};
  }
  return($J);
}


sub query_incr($$$$) {
  my ($query_node, $PQ, $J, $var) = @_;
  visit_incr($query_node, $query_node, $PQ, $J, $var);
}

sub visit_incr($$$$$) {
  my ($query_node, $visit_node, $PQ, $J, $var) = @_;
  foreach my $v (@{$visit_node->{L}}) {
    # if idom(v) is a strict ancestor of query_node...
    if ($v->{idom}->{dfs_num} < $query_node->{dfs_num}) {
      $J->{$v->{num}} = 1; # Add $v to the output set
      # phi function is of the form:
      # var := phi(var, ..., var);
      # where RHS has one var for each incoming edge.
      # The RHS will be filled in by rename_vars.
      # We don't need phi functions in the end node
      # (Every assigned variable would have one because of the
      # dummy link from start to end. For data flow tracking
      # we need to check the predecessors of the end node,
      # other than the start node and any proc return nodes,
      # in order to find the right name for $var):
      $v->{phi}->{$var} = [] unless ($v->{num} == 0);
      if (!$v->{mark}) {
	$v->{mark} = 1;
	# Insert v into PQ
	push(@{$$PQ[$v->{level}]}, $v);
      }
    } else {
      last;
    }
  }
  if (!$visit_node->{bndry}) {
    foreach my $C (@{$visit_node->{rev_idom}}) {
      visit_incr($query_node, $C, $PQ, $J, $var) unless ($C->{mark});
    }
  }
}


# After placing phi functions for all assigned variables,
# we can rename variables so that each variable is asssigned
# at most once.
# var__0 is the initial value of var.
# For each var we need a stack of current names plus a "next free name" variable.
# $stack->{$var} is a reference to the stack of names,
# $count->{$var} records how many assigns to the var have been seen

sub rename_vars($) {
  my ($start) = @_;
  rename_vars_sub($start, {}, {});
}


# Return the top of a stack (passed as an array ref)
# If the stack is empty or undef, then return default value

sub top($$) {
  my ($stack, $default) = @_;
  return($default) if (!defined($stack) || !@$stack);
  return($$stack[$#$stack]);
}

# Rename a list of vars using the tops of the stacks (or append __0):

sub rename_list($$) {
  my ($list, $stack) = @_;
  foreach (@$list) {
    next unless defined($_);
    $_ = top($stack->{$_}, $_ . "__0");
  }
}


# Order of operations in a node is:
# (1) phi functions, (2) assignments, (3) test control vars
# since the test occurs *after* any assignments

sub rename_vars_sub($$$) {
  my ($n, $stack, $count) = @_;
  # Rename LHS of phi functions with new variables and push onto the stacks
  foreach my $var (keys %{$n->{phi}}) {
    $count->{$var}++;
    my $new = "${var}__$count->{$var}";
    push(@{$stack->{$var}}, $new);
    $n->{phi}->{$new} = $n->{phi}->{$var};
    delete $n->{phi}->{$var};
  }
  # Rename RHS of assigns using names from the tops of the stacks:
  foreach my $var (keys %{$n->{assigns}}) {
    rename_list($n->{assigns}->{$var}, $stack);
  }
  # Rename LHS of assigns with new variables and push onto the stacks
  foreach my $var (keys %{$n->{assigns}}) {
    $count->{$var}++;
    my $new = "${var}__$count->{$var}";
    push(@{$stack->{$var}}, $new);
    $n->{assigns}->{$new} = $n->{assigns}->{$var};
    delete $n->{assigns}->{$var};
  }
  if ($n->{control}) {
    rename_list($n->{control}, $stack);
  }
  # Visit each successor in turn and rename RHS of each phi function.
  # (NB: the LHS of this phi function may already have been renamed)
  # Note: the arguments to the phi function are sorted by
  # the preorder traversal of the rev_idom tree.
  foreach my $x (@{$n->{succs}}) {
    foreach my $var (keys %{$x->{phi}}) {
      (my $orig = $var) =~ s/__\d+$//;
      push(@{$x->{phi}->{$var}}, top($stack->{$orig}, $orig . "__0"));
    }
  }
  # Process each child recursively:
  foreach my $child (@{$n->{rev_idom}}) {
    rename_vars_sub($child, $stack, $count);
  }
  # Pop the stack for each assigned variable:
  foreach my $var (keys %{$n->{phi}}) {
    $var =~ s/__\d+$//;
    pop(@{$stack->{$var}});
  }
  foreach my $var (keys %{$n->{assigns}}) {
    $var =~ s/__\d+$//;
    pop(@{$stack->{$var}});
  }
}


# Undo the effect of rename_vars,
# trim __\d+ from the end of each var name:

sub restore_vars($) {
  my ($bb) = @_;
  foreach my $n (@$bb) {
    delete $n->{phi};
    foreach (@{$n->{control}}) { s /__\d+$// };
    foreach my $type (qw(phi assigns)) {
      foreach my $var (keys %{$n->{$type}}) {
	foreach (@{$n->{$type}{$var}}) { s /__\d+$// };
	if ((my $new = $var) =~ s/__\d+$//) {
	  $n->{$type}{$new} = $n->{$type}{$var};
	  delete $n->{$type}{$var};
	}
      }
    }
  }
}


# Preprocess structures to expand structures into the complete
# set of fields (includes byte references).
#
# If the *.dat file is available, then we know when the whole
# structure has been written, eg:
# FOO[1] := A; FOO[2] := B; FOO[3] := C; FOO[4] := D
# when FOO is a four byte field.
#
# We can avoid the expansion by inserting update, access,
# update_seg and access_seg calls in the WSL before computing basic blocks.
#
# NB: If we are applying the results directly back to the Assembler file,
# then it doesn't matter if the ssa file gets large (due to expanding
# lots of structure references).

sub struct_preprocess($$$) {
  my ($bb, $dat_file, $max_fields) = @_;
  # Read the dat file and compute the children of each structure:
  local (*IN, $_);
  my %child = ();
  my %full_length = ();
  # Empty dat file means we are probably processing an X86 file:
  if ($dat_file eq "") {
    # Set up %child for [abcd]x: $child{ax}{1} = 2 etc.
    foreach my $reg (qw(ax bx cx dx)) {
      foreach my $byte (1, 2, 3) {
	$child{$reg}{$byte} = 2;
	$full_length{$reg} = 3;
      }
    }
  } else {
    open(IN, $dat_file) or die "Can't read *.dat file `$dat_file': $!\n";
    while (<IN>) {
      next if (/^#/);    # comment lines
      next if (/^\s*$/); # blank lines
      if (my ($key, $symbol, $value) = /^(\S+)\s+(\S+) = (.*)$/) {
	$value =~ s/^"(.*)"$/$1/;       # Strip enclosing quotes (if any)
	$value =~ s/\\n/\n/g;
	if ($key eq "Parent") {
	  $child{$value}{$symbol} = 1;
	} elsif ($key eq "Full_Length") {
	  $full_length{$symbol} = $value;
	}
      } else {
	die "Invalid *.dat file syntax on line $. of $dat_file:\n$_";
      }
    }
  }

  # Check which structures and fields appear in the file.
  # We need to know if a struct name appears by itself,
  # we also need to know which components of a struct are referenced.
  #
  my %seen = ();
  my %fullname = ();
  my %warned = ();
  foreach my $n (@$bb) {
    foreach my $key (@{$n->{control}}, keys %{$n->{assigns}},
		     map { @{$n->{assigns}{$_}} } keys %{$n->{assigns}}) {
      # Copy the name since we want to do substitutions on it
      # without affecting the names in the nodes!
      my $name = $key;
      if ($name =~ s/\[(\d+)\]$//) {
	# byte access
	my $byte = $1;
	# Treat foo[0] as foo[1] since byte zero is used for "others" below:
	$byte = 1 unless $byte;
	$name =~ s/^.*\.//;
	$child{$name}{$byte} = 2;
	if (!defined($full_length{$name})) {
	  (my $short = $name) =~ s/^.*__//;
	  $short = restore_dots($short);
	  if (defined($full_length{$short})) {
	    $full_length{$name} = $full_length{$short};
	  } else {
            # Don't assume anything about the length (was 4):
	    $full_length{$name} = 9999;
	  }
	}
	if (($name ne "a") && (($byte < 1) || ($byte > $full_length{$name}))) {
	  warn "$name" . "[$byte] is outside defined range of $name!\n"
	    unless $warned{$name,$byte}++;
	}
      } elsif ($name =~ /\.(\w+)$/) {
	my $field = $1;
	$fullname{$field} = $name;
	$name =~ s/\.\w+$//;
	$name =~ s/^.*\.//;
	$seen{$field}++;
	$child{$name}{$field} = 2;
      } else {
	$fullname{$name} ||= $name;
	$seen{$name}++;
      }
    }
  }

  # If a symbol has been seen and has children,
  # then create a list of the seen children.
  # Add the special child "STRUCT_others" if other fields
  # are defined but not referenced.
  # (We have to include the struct name to make it globally unique
  # so that the $fullname{$child} table works properly).
  # NB: For DSECT structures we *always* add this special child
  # since an assignment to the DSECT pointer must be preserved
  # if any DSECT element is referenced or even written to!
  # An assignment to the DSECT pointer will update STRUCT_others,
  # and so will be preserved, even if every (other) DSECT field
  # is subsequently overwritten.
  #
  # Add dummy byte reference 0 if bytes are referenced
  # but not the whole defined area.
  # Then we can expand each struct reference to its list of children.

  # A mixture of byte references and field references on the same symbol
  # will cause extra aliasing problems!

  my %children = ();
  my %no_bytes = (); # Names for which we delete the byte references
  foreach my $name (keys %seen) {
    if (defined($child{$name})) {
      # Struct has been seen and has children (defined or referenced).
      # Build the list of children/bytes that have been referenced:
      $children{$name} = [ grep { $child{$name}{$_} == 2 } keys %{$child{$name}} ];
      if (!@{$children{$name}}) {
	# Delete empty child lists
	# (children are defined in the *.dat file but none are referenced):
	delete $children{$name};
      } else {
	# Check that all children are byte numbers
	# or all are field names:

        if ((grep { /^\d+$/ } @{$children{$name}})
	    && (grep { /\D/ } @{$children{$name}})) {
          $no_bytes{$name}++;
          @{$children{$name}} = grep { /\D/ } @{$children{$name}};
	}

	if (grep { /^\d+$/ } @{$children{$name}}) {
	  if (grep { /\D/ } @{$children{$name}}) {
	    warn "$name has both byte references and field name references!\n";
	  } else {
	    # Add *others* field (byte zero) if byte references don't cover struct:
	    foreach my $byte (1..$full_length{$name}) {
	      if (!$child{$name}{$byte}) {
		push(@{$children{$name}}, 0);
		last;
	      }
	    }
	  }
	}
	if (grep { /\D/ } @{$children{$name}}) {
	  foreach my $field (keys %{$child{$name}}) {

	    push(@{$children{$name}}, "${name}_others");
            $fullname{"${name}_others"} = "$fullname{$name}.${name}_others";
	    last;

	    #if ($child{$name}{$field} != 2) {
	    #  push(@{$children{$name}}, "${name}_others");
	    #  $fullname{"${name}_others"} = "$fullname{$name}.${name}_others";
	    #  last;
	    #}
	  }
	}
      }
    }
  }

  # Compute a version of %children which has full names:
  my %full_children = ();
  foreach my $name (keys %children) {
    my $fullname = $fullname{$name};
    foreach my $child (@{$children{$name}}) {
      if ($child =~ /^\d+$/) {
	push(@{$full_children{$fullname}}, $fullname . "[$child]");
      } else {
	push(@{$full_children{$fullname}}, "$fullname.$child");
	warn "$fullname{$child} is not $fullname.$child (name = $name) !!\n"
	  unless ($fullname{$child} eq "$fullname.$child");
      }
    }
  }

  # Now expand any reference to $struct to @{$full_children{$struct}}:

  my @structs = keys %full_children;
  my %expand = (); # lists all lowest-level descendants of each variable, or itself
  foreach my $struct (@structs) {
    $expand{$struct} = expand([$struct], \%full_children);
    if (@{$expand{$struct}} > $max_fields) {
      print STDERR "$struct has too many children (",
	      scalar(@{$expand{$struct}}), ")\n";
      $expand{$struct} = [$struct];
    }
  }


  # PROC CALL 1 nodes have vars prefixed with the proc name
  # Add these to %expand if the base name has children:

  my %done_proc = ();
  foreach my $n (@$bb) {
    next unless ($n->{type} =~ /^PROC CALL 1 (.*)$/);
    my $name = $1;
    my $base = "";
    foreach my $var (keys %{$n->{assigns}}) {
      ($base = $var) =~ s/^${name}__// or next;
      $base =~ s/_dot_/\./g;
      next unless $full_children{$base};
      next if $expand{$var};
      $expand{$var} = [map { "${name}__" . fix_dots($_) } @{$expand{$base}}];
    }
  }



  foreach my $n (@$bb) {
    $n->{control} = remdups($n->{control}, \%expand);
    # Assigns in PROC CALL/Return node are copies of the form foo := foo:
    if ($n->{type} =~ /^PROC (Return|CALL [13])/) {
      my %new_assigns = ();
      foreach my $var (keys %{$n->{assigns}}) {
        if (!$full_children{$var}) {
	  $new_assigns{$var} = $n->{assigns}{$var};
	  next;
	}
	my $RHS = remdups($n->{assigns}{$var}, \%expand);
	my @RHS = sort @$RHS;
	if (!@RHS) {
	  $new_assigns{$var} = $n->{assigns}{$var};
	  next;
	}
	foreach my $child (sort @{$expand{$var}}) {
          die unless @RHS;
	  $new_assigns{$child} = [shift @RHS];
	}
      }
      $n->{assigns} = \%new_assigns;
    } else {
#print STDERR "\n======= Node $n->{num} =============\n";
      foreach my $var (keys %{$n->{assigns}}) {
	my $RHS = remdups($n->{assigns}{$var}, \%expand);
	if ($full_children{$var}) {
#print STDERR "Deleting assign $var = @{$n->{assigns}{$var}}\n";
	  delete $n->{assigns}{$var};
	  # NB: If new RHS list is large and $var also
	  # expands to a large list, then reduce the size
	  # of the result by introducing an extra temp assignment
	  # and assigning tmp to each child.
	  # BUT this assignment has to go into an extra, separate node!
	  #if (@{$RHS} > 1) {
	  #  $n->{assigns}{"*tmp*"} = $RHS;
	  #  $RHS = ["*tmp*"];
	  #}
	  foreach my $child (@{$expand{$var}}) {
	    # Copy the list for each assignment:
	    $n->{assigns}{$child} = [@$RHS];
#print STDERR "  Adding assign $child = @$RHS\n";

	  }
	} else {
	  $n->{assigns}{$var} = $RHS;
#print STDERR "Modifying assign to $var = @$RHS\n";
	}
      }
    }
  }
}


# Recursively expand a list by replacing a struct by its children
# and removing duplicates:

sub expand($$) {
  my ($list, $children) = @_;
  local $_;
  if (grep { $children->{$_} } @$list) {
    my %seen = ();
    return [ grep { !$seen{$_}++ }
	     map { $children->{$_} ? @{expand($children->{$_}, $children)} : $_ }
	     @$list ];
  } else {
    return $list;
  }
}

# Form the union of a list of array refs, removing duplicates:
sub remdups($$) {
  my ($list, $expand) = @_;
  my %seen = ();
  my @result = ();
  foreach my $var (@$list) {
    if (!$$expand{$var}) {
      $$expand{$var} = [$var];
    }
    push(@result, grep { !$seen{$_}++ } @{$$expand{$var}});
  }
  return(\@result);
}


# In some cases, struct_preprocess may make the SSA file too big
# due to many byte references. Also, byte references cannot be renamed
# in the WSL file.  So this routine deletes byte references
# by converting them to an update or access of the main field.
# This version only processes byte references of registers
# (since we might want to track individual bytes for other fields,
# but we can treat a byte reference on a register as a simple update).

my $reg_byte = qr/^(r\d+)\[\d+\]$/;

sub byte_preprocess($) {
  my ($bb) = @_;
  foreach my $n (@$bb) {
    $n->{control} = byte_refs($n->{control})
      if grep { /$reg_byte/ } @{$n->{control}};
    foreach my $var (keys %{$n->{assigns}}) {
      if ($var =~ /$reg_byte/) {
	my $new = $1;
	$n->{assigns}{$new} ||= [];
	$n->{assigns}{$new}
	  = byte_refs [ @{$n->{assigns}{$new}}, $new, @{$n->{assigns}{$var}} ];
	delete $n->{assigns}{$var};
      } else {
	$n->{assigns}{$var} = byte_refs($n->{assigns}{$var})
	  if grep { /$reg_byte/ } @{$n->{assigns}{$var}};
      }
    }
  }
  # TODO: Check for byte sequences which always occur together and collapse them
}


# Convert register byte references to refer to the whole register
# and also delete duplicates:

sub byte_refs($) {
  my ($list) = @_;
  my %seen = ();
  return [ grep { !$seen{$_}++ }
	   map { /$reg_byte/ ? $1 : $_ }
	   @$list ];
}


# A reference to foo.bar where foo is a register
# has to be replaced by an update of foo *and os*,
# otherwise bar does not have a unique fullname.

# The assignment to os is needed so that the first assignment to bx
# in the following does not get deleted:
# bx := foo;
# a[bx].bar := 0;
# bx := baz

sub register_preprocess($$) {
  my ($bb, $regs) = @_;
  foreach my $n (@$bb) {
    $n->{control} = reg_field_refs($n->{control}, $regs)
      if grep { /^(.*?)\./ && $$regs{$1} } @{$n->{control}};
    foreach my $var (keys %{$n->{assigns}}) {
      if (($var =~ /^(.*?)\./) && $$regs{$1}) {
	my $new = $1;
	$n->{assigns}{$new} ||= [];
	$n->{assigns}{$new}
	  = reg_field_refs [ @{$n->{assigns}{$new}}, $new, @{$n->{assigns}{$var}} ], $regs;
        # Add an assignment to os:
        $n->{assigns}{os} = $n->{assigns}{$new};
	delete $n->{assigns}{$var};
      } else {
	$n->{assigns}{$var} = reg_field_refs($n->{assigns}{$var}, $regs)
          if grep { /^(.*?)\./ && $$regs{$1} } @{$n->{assigns}{$var}};
      }
    }
  }
}


# Convert register field references to refer to the whole register
# and also delete duplicates:

sub reg_field_refs($$) {
  my ($list, $regs) = @_;
  my %seen = ();
  return [ grep { !$seen{$_}++ }
	   map { (/(.*?)\./ && $$regs{$1}) ? $1 : $_ }
	   @$list ];
}


# This requires that the cdequiv_header and conds links have been set up.
# (But only if the blocks include procedure calls).
# If $n->{num} == $n->{cdequiv_header} then
# $n->{conds} is the list of nodes generated from conds_query($n, $A)

sub calculate_proc_summaries($$$$) {
  my ($bb, $start, $end, $use_cd) = @_;
  # We assume that proc names are unique: ie only one definition of each proc.
  # First initialise all proc call summaries to blank,
  # and record proc headers, return points and calls.
  my %proc_calls = ();
  my %proc_head = ();
  my %proc_body = ();
  my %proc_return = ();
  foreach my $n (@$bb) {
    if ($n->{type} =~ /^PROC CALL 2 (\S+)$/) {
      push(@{$proc_calls{$1}}, $n);
    } elsif ($n->{type} =~ /^PROC Header (\S+)$/) {
      die "Multiple definitions of proc $1 " if defined($proc_head{$1});
      $proc_head{$1} = $n;
    } elsif ($n->{type} =~ /^PROC Return (\S+)/) {
      die "Multiple returns of proc $1 " if defined($proc_return{$1});
      $proc_return{$1} = $n;
    }
  }
  # Find the bodies of each proc:
  foreach my $proc (keys %proc_head) {
    # A proc which always exits abnormally:
    $proc_return{$proc} ||= $end;
    $proc_body{$proc} = get_proc_body($proc_head{$proc}, $proc_return{$proc}, $end);
  }
  # Compute the proc call graph:
  my %calls = ();
  my $name;
  foreach my $head (values %proc_head) {
    $head->{type} =~ /^PROC Header (\S+)$/ or die;
    $name = $1;
    foreach my $n (@{depth_first_list($head, "succs", "dfs_succs_num", "dfs_succs_p")}) {
      if ($n->{type} =~ /PROC CALL 2 (\S+)$/) {
	$calls{$name}{$1}++;
      }
    }
  }

  # Clear all existing proc call summaries:
  my %summ = ();
  my $summ;
  foreach $name (keys %proc_head) {
    $summ{$name} = {};
    foreach my $n (@{$proc_calls{$name}}) {
      $n->{control} = [];
      $n->{assigns} = {};
    }
  }

  # Set up the right dfs_num values for phi_placement:
  depth_first_list($start, "rev_idom", "dfs_num", "dfsp");

  # Summarise each proc body, summarise called procs first
  # (apart from recursive calls)
  # If recursion is detected, then iterate to convergence.

  # First, create a depth-first list of procs
  # Create a dummy main proc with calls to all procs:
  my $main = "**main**";
  foreach (keys %proc_head) { $calls{$main}{$_}++ };
  $SSA::recursion = 0;
  %SSA::done = ();
  my $path = [$main];
  my @dfs = ();
  dfs_list(\@dfs, $path, $main, \%calls);
  pop(@dfs); # pop the dummy $main
#print STDERR "DFS = ", join(", ", @dfs), "\n" if (@dfs);
  $end->{level} = 1;
  for (;;) {
    # Check if any summary changed:
    my $changed = 0;
    # Summarise procs in dfs order:
    foreach my $proc (@dfs) {
#print STDERR "summarise: $proc\n";
      $summ = summarise_proc($proc, $proc_head{$proc}, $proc_return{$proc},
			     $proc_body{$proc}, $use_cd, $end);
      if (different_assigns($summ, $summ{$proc})) {
	$changed++;
	$summ{$proc} = $summ;
	# update all of $proc_calls{$proc} with the new summary:
	foreach my $n (@{$proc_calls{$proc}}) {
	  $n->{assigns} = prefix_assigns($summ, $proc);
	}
      }
    }

    last if !$SSA::recursion;
    last if !$changed;
#print STDERR "Re-iterating...\n";
  }
  return(\%summ);
} # end of calculate_proc_summaries


# For an accurate SSA when there are procedures,
# compute control dependencies and SSA preprocessing
# and call compute_proc_summaries

sub calculate_SSA($$) {
  my ($bb, $start) = @_;
  my %assigns = ();
  foreach my $n (@$bb) {
    undef $n->{level};
    # Don't need phi functions for PROC CALL 1 and 2 assignments:
    next if ($n->{type} =~ /^PROC CALL [12] /);
    foreach my $var (keys %{$n->{assigns}}) {
      push(@{$assigns{$var}}, $n);
    }
  }
  # Set up the right dfs_num values for phi_placement:
  depth_first_list($start, "rev_idom", "dfs_num", "dfsp");
  foreach my $var (keys %assigns) {
    my $J = phi_placement($bb, $start, $assigns{$var}, $var);
  }
  # Delete a phi function if the variable is clobbered in the same node:
  foreach my $n (@$bb) {
    foreach my $var (keys %{$n->{phi}}) {
      # Check for an assignment to the same variable:
      next unless $n->{assigns}->{$var};
      # Skip if the variable is assigned from its original value
      # (in which case the phi function is still needed):
      next if grep { $_ eq $var } @{$n->{assigns}{$var}};
      # This phi function result is clobbered by an assign to the same variable,
      # so delete the phi function:
      delete $n->{phi}{$var};
    }
  }
  rename_vars($start);
}


# Construct a summary of the given proc: track control and data dependencies
# from each output variable in the return node back to the header node.

# If $use_cd is false, then don't track control dependency links.

sub summarise_proc($$$$$$) {
  my ($proc, $head, $return, $body, $use_cd, $end) = @_;
#print STDERR "\nSummarise $proc: ", join(", ", map { $_->{num} } @$body), "\n";
  # Convert the proc to SSA form:
  my %assigns = ();
  return(\%assigns) if $return->{num} == $end->{num};
  foreach my $n (@$body) {
    undef $n->{level};
    # Don't need phi functions for PROC CALL 1 and 2 assignments:
    next if ($n->{type} =~ /^PROC CALL [12] /);
    foreach my $var (keys %{$n->{assigns}}) {
      push(@{$assigns{$var}}, $n);
    }
  }
  foreach my $var (keys %assigns) {
    my $J = phi_placement($body, $head, $assigns{$var}, $var);
  }
  rename_vars($head);

  # Find the unique assignment for each assigned variable:
  my %assign = ();
  foreach my $n (@$body) {
    foreach my $type (qw(assigns phi)) {
      foreach my $var (keys %{$n->{$type}}) {
	die "$var is assigned more than once in SSA of $proc!\n"
	  if defined($assign{$var});
	$assign{$var} = $n;
      }
    }
  }

  # Follow data and control dependencies from $return to $head
  my %summ = (); # Used to store the results
  my @todo_v;    # Stack of vars to track
  my @todo_n;    # Stack of nodes to track
  my %done;      # var,node combinations already done (or on @todo) for $var
  # IF v,n is on the stack, then v is assigned in n or a control var of n.
  my $head_num = $head->{num};
  my ($v, $n, $m, $vars, $m_num);
  foreach my $var (keys %{$return->{assigns}}) {
#print STDERR "Tracking $var:\n";
    # Ensure $var appears in the summary even if it is assigned from a constant
    # (ie not dependent on any input variables):
    $summ{$var} = {};
    @todo_v = ($var);
    @todo_n = ($return);
    %done = ();
    $done{$var,$return->{num}}++;
    $assigns{$var} = {};
    while (@todo_v) {
      $v = pop(@todo_v); $n = pop(@todo_n);
#print STDERR "  tracking $v in $n->{num}\n";
      # Check if we have reached the header node:
      if ($n->{num} == $head_num) {
#print STDERR " $v" unless $summ{$var}{$v};
	$summ{$var}{$v}++;
	next;
      }
      # Control Dependencies:
      # For all control dependent nodes of $n (conds)
      # add each control variable.
      # Any new results are marked done and pushed onto @todo_v and @todo_n
      if ($use_cd) {
        foreach my $m (@{$n->{cdequiv_header}->{conds}}) {
          $m_num = $m->{num};
          foreach my $v (@{$m->{control}}) {
            if (!$done{$v,$m_num}++) {
#print STDERR "    need control var $v in $m_num\n";
              push(@todo_v, $v); push(@todo_n, $m);
            }
          }
        }
      }
      # Data Dependencies:
      # Apply any assignments and phi functions in the node to the variable
      # to get the list of referenced variables.
      # (NB if $v is a control var with no assigns, then this preserves $v):
      $vars = [$v];
      $vars = apply_assign($vars, $n->{assigns});
      $vars = apply_assign($vars, $n->{phi});
      # For each referenced variable, find the single assignment
      # for the variable (if any) and add to the stacks:
      foreach my $v (@$vars) {
	next unless defined($assign{$v});
	$m = $assign{$v};
	$m_num = $m->{num};
	if (!$done{$v,$m_num}++) {
#print STDERR "    $v assigned in $m_num\n";
	  push(@todo_v, $v); push(@todo_n, $m);
	}
      }
    } # next @todo_v
#print STDERR "\n";
  } # next $var
  # Convert the hash refs in %summ to lists:
  foreach my $var (keys %summ) {
    (my $new = $var) =~ s/__\d+$// or die;
    $summ{$new} = [sort keys %{$summ{$var}}];
    foreach (@{$summ{$new}}) { s/__\d+$// };
    delete $summ{$var};
  }
  restore_vars($body);

#print STDERR "Summary for $proc:\n";
#foreach my $var (sort keys %summ) {
#  print STDERR "$var := <", join(", ", @{$summ{$var}}), ">\n";
#}
#print STDERR "------\n";

  return(\%summ);
}


# Create a depth first list of called procs
# by pushing them onto the given array ref.
# Push a proc's non-recursive children before pushing the proc.
# Use %SSA::done to avoid repetition.
# Use @$path to detect recursion.
# Record detected recursion in $SSA::recursion
# $proc is at the top of $path and is marked in %done

sub dfs_list($$$$) {
  my ($dfs, $path, $proc, $calls) = @_;
  foreach my $child (keys %{$calls->{$proc}}) {
    if (grep { $_ eq $child } @$path) {
      # This child is a recursive call
      $SSA::recursion++;
    } elsif ($SSA::done{$child}++) {
      # Already done this one
      next;
    } else {
      push(@$path, $child);
      dfs_list($dfs, $path, $child, $calls);
      pop(@$path);
    }
  }
  push(@$dfs, $proc);
}


# Find the list of nodes which constitute the body
# of the given proc:

sub get_proc_body($$$) {
  my ($head, $return, $end) = @_;
  my @body = ();
  my %mark = ();
  my @todo = ($head);
  $mark{$head->{num}}++;
  $mark{$return->{num}}++;
  my $n;
  while (@todo) {
    $n = pop(@todo);
    # Don't include the end node if we branch out to it:
    next if $n->{num} == $end->{num};
    push(@body, $n);
    push(@todo, grep { !$mark{$_->{num}}++ } @{$n->{succs}});
  }
  push(@body, $return);
#print STDERR $head->{type}, ": ", join(", ", map { $_->{num} } @body), "\n";
  return(\@body);
}



# Apply the given assignment (hashref) to the list of variables
# Replace any assigned variable in the list by its dependent variables:

sub apply_assign($$) {
  my ($list, $assign) = @_;
  return [ map { defined($assign->{$_}) ? @{$assign->{$_}} : $_ } @$list ];
}


# Check if two assign hashrefs differ:

sub different_assigns($$) {
  my ($x, $y) = @_;
  return(1) if join(" ", sort keys %$x) ne join(" ", sort keys %$y);
  foreach my $var (keys %$x) {
    return(1) if join(" ", sort @{$x->{$var}}) ne join(" ", sort @{$y->{$var}});
  }
  return(0);
}


# Prefix all vars in the hash table with the given string
# (also creates a new hash since
# we need separate hashes for each proc call node)
# Also convert dots to underscores
# (The prefixing converts a variable name to a temp name used in PROC CALL nodes.
# This temp name must be a simple variable, not a field reference,
# since each field name must have a unique parent.)

sub prefix_assigns($$) {
  my ($assigns, $name) = @_;
  my $new = {};
  foreach my $var (keys %$assigns) {
    $new->{"${name}__" . fix_dots($var)} = [map { "${name}__" . fix_dots($_) }
					        @{$assigns->{$var}}];
  }
  return($new);
}


sub fix_dots($) {
  my ($str) = @_;
  $str =~ s/\./_dot_/g;
  return($str);
}


sub restore_dots($) {
  my ($str) = @_;
  $str =~ s/_dot_/\./g;
  return($str);
}




1;
