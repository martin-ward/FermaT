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
# Slice a basic blocks file in SSA format
#
#

package Slice;
require 5.000;
require Exporter;
use Carp;

@ISA = qw(Exporter);
@EXPORT = qw(find_assign_refs slice find_nodes var_posn);

use Control_Dep;
use strict;
use warnings;

sub find_nodes($$$$$$);

# Return hash tables of the (unique) assignment node for a variable
# and the set of reference nodes for a variable:

sub find_assign_refs($) {
  my ($bb) = @_;
  my %assign = ();
  my %refs = ();
  foreach my $n (@$bb) {
    foreach my $type (qw(assigns phi)) {
      foreach my $var (keys %{$n->{$type}}) {
	die "$var is assigned more than once in SSA file!\n"
	  if defined($assign{$var});
	$assign{$var} = $n;
	foreach my $v (@{$n->{$type}{$var}}) {
          next unless defined($v);
	  push(@{$refs{$v}}, $n);
	}
      }
    }
    # References to control variables:
    foreach my $v (@{$n->{control}}) {
      push(@{$refs{$v}}, $n);
    }
  }
  return(\%assign, \%refs);
}


# Given a set of blocks in SSA format plus a @todo list,
# plus a pair of direction indicators ($forwards, $backwards)
# to show which direction(s) to slice in.
# Return a %done table with all var/node pairs in the slice.

# Do a forwards/backwards reachability on node/var pairs from @todo.
# When we reach a proc call we track across the proc summary and into
# the proc body. We never need to track out of a proc body.

# Backward slices (the default) follow return nodes to track into a proc body:
#   -- from the PROC CALL 3 node to the Proc Return node
# Forward slices follow proc call nodes (in reverse) to track into a proc body:
#   -- from the PROC CALL 1 node to the Proc Header node

sub slice($$$$$$$) {
  my ($start, $bb, $refs, $assign, $forwards, $backwards, $V) = @_;
  my @todo = @$start;

  # Find the unique assignment for each assigned variable
  # and all the references for each variable:
  my %assign = ();
  my %refs = ();
  my %proc_return = ();
  my %proc_header = ();
  my %proc_calls1 = ();
  my %proc_calls2 = ();
  my %proc_calls3 = ();
  foreach my $n (@$bb) {
    if ($n->{type} =~ /^PROC Return (.*)$/) {
      $proc_return{$1} = $n;
    } elsif  ($n->{type} =~ /^PROC Header (.*)$/) {
      $proc_header{$1} = $n;
    } elsif  ($n->{type} =~ /^PROC CALL 1 (.*)$/) {
      push(@{$proc_calls1{$1}}, $n);
    } elsif  ($n->{type} =~ /^PROC CALL 2 (.*)$/) {
      push(@{$proc_calls2{$1}}, $n);
    } elsif  ($n->{type} =~ /^PROC CALL 3 (.*)$/) {
      push(@{$proc_calls3{$1}}, $n);
    }
  }

  my %done = ();

  # Add the @todo pairs to %done
  my ($v, $n, $cs);
  my @tmp = @todo;
  while (@tmp) {
    $cs = pop(@tmp); $n = pop(@tmp); $v = pop(@tmp);
    print STDERR "TODO: $v at $n->{num}\n" if $V;
    $done{$v,$n}++;
  }

  # Order of operations in a node is:
  # (1) phi functions, (2) assignments, (3) test control vars
  # since the test occurs *after* any assignments

  while (@todo) {
    $cs = pop(@todo); $n = pop(@todo); $v = pop(@todo);
print STDERR "Tracking ", var_posn($v, $n), "\n" if $V;

    if ($backwards) {
      # Backwards Control Dependencies:

      # For all control dependent nodes of $n
      # add each control variable
      foreach my $m (@{$n->{cdequiv_header}->{conds}}) {
	foreach my $v (@{$m->{control}}) {
	  if (!$done{$v,$m}++) {
print STDERR "    need control var ", var_posn($v, $m), "\n" if $V;
	    push(@todo, $v, $m, $cs);
	  }
	}
      }

      # Backwards Data Dependencies: assignments and phi functions

      # If $v is assigned in this node, then add all the referenced vars
      # (with their unique assign nodes)
      # If $v is assigned in another node, then add the $v and the assigning node.

      foreach my $type ("assigns", "phi") {
	if (defined($n->{$type}{$v})) {
	  foreach my $u (@{$n->{$type}{$v}}) {
	    next if ($u =~ /__0$/); # No assignment for this variable
	    if (!$done{$u,$$assign{$u}}++) {
print STDERR "    need var ", var_posn($u, $$assign{$u}), "\n" if $V;
	      push(@todo, $u, $$assign{$u}, $cs);
	    }
	  }
	}
	if (($v !~ /__0$/) && !$done{$v,$$assign{$v}}++) {
print STDERR "    assigned var ", var_posn($v, $$assign{$v}), "\n" if $V;
	  push(@todo, $v, $$assign{$v}, $cs);
	}
      }

      # Backwards Proc Call Dependencies

      # Track the required formal output parameters
      # back to the return node of the proc body
      if ($n->{type} =~ /^PROC CALL 2 (.*)$/) {
	my $name = $1;
	$cs = [$name, @$cs];
	my $m = $proc_return{$name};
	my $vv = remove_prefix($v, $name);
	foreach my $u (keys %{$m->{assigns}}) {
	  if ($u =~ /^\Q${vv}\E__\d+$/) {
	    # Track $u backwards through the proc body
	    if (!$done{$u,$m}++) {
print STDERR "    track PROC Return var ", var_posn($u, $m), "\n" if $V;
	      push(@todo, $u, $m, $cs);
	    }
	  }
	}
      }

      # Track from header nodes of procs we didn't enter
      # back to all PROC CALL 1 nodes:
      if (($n->{type} =~ /^PROC Header (.*)$/) && !grep { $_ eq $1 } @$cs) {
	my $name = $1;
	my $vv = add_prefix($v, $name);
	foreach my $m (@{$proc_calls1{$name}}) {
	  foreach my $u (keys %{$m->{assigns}}) {
	    if ($u =~ /^\Q${vv}\E__\d+$/) {
              foreach my $v (@{$m->{assigns}{$u}}) {
	        # Track $v backwards from the proc call
	        if (!$done{$v,$m}++) {
print STDERR "    track PROC CALL 1, vv = $vv, var = ", var_posn($v, $m), "\n" if $V;
		  push(@todo, $v, $m, $cs);
                }
	      }
	    }
	  }
	}
      }
    }

    if ($forwards) {
      # Forwards Control Dependencies:

      # If $v is a control variable, then pull in all
      # the assigned variables in all cd_set nodes for each outgoing edge:

      if (grep { $_ eq $v } @{$n->{control}}) {
	foreach my $s (@{$n->{succs}}) {
	  # Process cd_set of edge $n --> $s:
	  foreach my $m (@{cd_set($n, $s)}) {
	    foreach my $type ("phi", "assigns") {
	      foreach my $u (keys %{$m->{$type}}) {
		if (!$done{$u,$m}++) {
print STDERR "    need controlled var ", var_posn($u, $m), "\n" if $V;
		  push(@todo, $u, $m, $cs);
		}
	      }
	    }
	  }
	}
      }

      # Forwards Data Dependencies

      # If $v is assigned in this node, then add all the references to $v
      # If $v is referenced in this node, then add the assigned variable.

      foreach my $type ("phi", "assigns") {
	if (defined($n->{$type}{$v})) {
	  foreach my $m (@{$$refs{$v}}) {
	    if (!$done{$v,$m}++) {
print STDERR "    need var ", var_posn($v, $m), "\n" if $V;
	      push(@todo, $v, $m, $cs);
	    }
	  }
	}
	foreach my $u (keys %{$n->{$type}}) {
	  if (grep { $_ eq $v } @{$n->{$type}{$u}}) {
	    # Assignment to $u in $n references $v
	    if (!$done{$u,$n}++) {
print STDERR "    need var ", var_posn($u, $n), "\n" if $V;
	      push(@todo, $u, $n, $cs);
	    }
	  }
	}
      }

      # Forwards Proc Call Dependencies

      # Track the input parameters into the header node of the proc body
      if ($n->{type} =~ /^PROC CALL 2 (.*)$/) {
	my $name = $1;
	$cs = [$name, @$cs];
	my $m = $proc_header{$name};
	my $vv = remove_prefix($v, $name);
	foreach my $u (keys %{$m->{assigns}}) {
	  if ($u =~ /^\Q${vv}\E__\d+$/) {
	    # Track $u forwards through the proc body
	    if (!$done{$u,$m}++) {
print STDERR "    track PROC Header $name, var ", var_posn($u, $m), "\n" if $V;
	      push(@todo, $u, $m, $cs);
	    }
	  }
	}
      }
      # Track from return nodes of procs we didn't enter
      # on to all PROC CALL 3 nodes.
      # Instead of tracking each assigned variable in the PROC CALL 3 node,
      # we track forwards from the assignment in the PROC CALL 2 node:
      if (($n->{type} =~ /^PROC Return (.*)$/) && !grep { $_ eq $1 } @$cs) {
	my $name = $1;
	my $vv = add_prefix($v, $name);
	foreach my $m (@{$proc_calls2{$name}}) {
	  foreach my $u (keys %{$m->{assigns}}) {
	    if ($u =~ /^\Q${vv}\E__\d+$/) {
	      # Track $u forwards from the proc call
	      if (!$done{$u,$m}++) {
print STDERR "    track PROC CALL 2 $name, var ", var_posn($u, $m), "\n" if $V;
		push(@todo, $u, $m, $cs);
	      }
	    }
	  }
	}
      }
    }
  }
print STDERR "\n" if $V;
  return(\%done);
}





# Find the static single assignment name(s) for the given variable
# at the given node and return the name and node.
# If the variable isn't assigned in the node, then search backwards up the idom tree
# (recall that rename_vars renames variables by searching
# downwards through the idom tree after phi functions have been placed).
# If the start node is reached then the new name is $v__0
# NB: The end node is a special case: look in the predecessors of $end
# other than $start and any PROC Return nodes.
# In this case there may be more than one SSA name, and node pair
# (where two or more data flows meet at the end node). 

sub find_nodes($$$$$$) {
  my ($v, $n, $start, $end, $refs, $V) = @_;
  print STDERR "Name of $v at $n->{num} = " if $V;
  if ($n eq $start) {
    print STDERR "${v}__0\n" if $V;
    return(map { ($v . "__0", $_, []) } @{$$refs{$v . "__0"}});
  }
  for (keys %{$n->{phi}}, keys %{$n->{assigns}}) {
    # Look for the name ($v) with an optional byte subscript and any SSA rename tag:
    if (/^\Q${v}\E(\[\d+\])?__\d+$/) {
      print STDERR "$_\n" if $V;
      return($_, $n, []);
    }
  }
  # For the end node, look in the predecessors other than $start
  # and any PROC Return nodes
  if ($n eq $end) {
    my @nodes = ();
    foreach my $m (@{$n->{preds}}) {
      next if ($m eq $start);
      next if ($m->{type} =~ /^PROC Return /);
      print STDERR "-- look in pred: $m->{num}\n" if $V;
      push(@nodes, find_nodes($v, $m, $start, $end, $refs, $V));
    }
    return(@nodes) if @nodes;
    # Return all the references to v__0
    print STDERR "${v}__0\n" if $V;
    return(map { ($v . "__0", $_, []) } @{$$refs{$v . "__0"}});
  } else {
    # Look in the immediate dominator for the variable:
    print STDERR "-- look in idom: $n->{idom}->{num}\n" if $V;
    return(find_nodes($v, $n->{idom}, $start, $end, $refs, $V));
  }
}



# Return a string with the variable name and its node number and WSL posn (if any):  
sub var_posn($$) {
  my ($v, $n) = @_;
  return "$v in $n->{num}" . (defined($n->{posn}) ? " at $n->{posn}" : "");
}


# Add a prefix to a variable, and convert dots to _dot_
# to ensure that the result is a simple variable.
# Both of these also remove the SSA code number from the end.

sub add_prefix($$) {
  my ($var, $name) = @_;
  $var =~ s/^(.*)__\d+$/${name}__$1/
    or die "add_prefix($var, $name) failed!";
  $var =~ s/\./_dot_/;
  return($var);
}

# Remove a prefix from a variable, and convert "_dot_" back to "."

sub remove_prefix($$) {
  my ($var, $name) = @_;
  $var =~ s/^${name}__(.*)__\d+$/$1/
    or die "remove_prefix($var, $name) failed!";
  $var =~ s/_dot_/\./g;
  return($var);
}



1;
