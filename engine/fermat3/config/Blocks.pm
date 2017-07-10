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
# Module with code to deal with basic blocks
#
# The keys for the hashes in the @$bb array are:
#
# num		 Index number ($$bb[$i]->{num} = $i)
# type		 Type string (see below)
# code		 WSL Code string (the label for the generated graph node)
# posn		 WSL posn of the node (space separated list of numbers)
# len		 Number of WSL items in this block
# links		 Links back to the listing (list of strings: line no + symbols)
# control	 Array ref of control elements (as strings)
# phi		 Hash ref of phi punctions (elt -> array ref of elts on RHS)
# assigns	 Hash ref of assignments (elt -> array ref of referenced elts)
# succs_n	 Array ref of successor block numbers
# succs		 Array ref of successors (hash refs)
# preds		 Array ref of predecessors
# dom		 Integer set of all dominators (calculated by the slow algorithm)
# $dfs_num	 The number of this node in the DFS search list
# $dfsp		 The parent node of this node in the DFS search tree, or []
# $bfs_num	 The number of this node in the BFS search list
# idom		 The immediate dominator of this node ($start->{idom} = [])
# ipdom		 The immediate postdominator of this node ($end->{ipdom} = [])
# lines		 Hash ref mapping each elt to a list of listing line numbers
# cdequiv_header The node in this nodes cdequiv set which records the dependencies
# conds          For a cdequiv_header node, array ref of controlling nodes
#
# An "element" is a variable name which may be a struct and/or a byte reference
# eg FOO.BAR[23]. In SSA form the element has "__N" appended, eg FOO.BAR[23]__3
#
# The following keys refer to values used locally in calculate_idom_fast
# and/or calculate_idom_fast2:
# sdom ancestor path_min samedom best
# bucket (ref to an array of nodes)

# Note: The keys to use for $dfs_num, $dfsp and $bfs_num
# are passed to depth_first_list and breadth_first_list
# since they depend on which set of edges is being followed.

# Note: "undefined" $dfsp and idom values are set to []
# so that set_to_list knows there is an error if it sees an undefined value

# Note: for any node $n
# 	$n->{succs} = [ map { $bb[$_] } @{$n->{succs_n}} ]
# For any integer $i
# 	$bb[$i]->{num} = $i


# The possible types for a node are:
#
# "block"
# "IF"
# "FLOOP Header"
# "WHILE Header"
# "WHERE Header"
# "PROC Header " ++  name of proc
# "PROC Return " ++  name of proc
# "PROC CALL " ++  name of proc
# "ACTION " ++ name of action
# "Save"
# "FOR Init"
# "FOR Header"
# "FOR Footer"
# "Restore"
#


package Blocks;
require 5.000;
require Exporter;
use Carp;
use Data::Dumper;

@ISA = qw(Exporter);
@EXPORT = qw(bb_read bb_write bb_callgraph calculate_reverse calculate_level
	     calculate_dom breadth_first_list depth_first_list
	     calculate_idom find_unreachable delete_unreachable
	     calculate_back_edges calculate_retreating_edges
	     calculate_idom_fast calculate_idom_fast2
	     set_to_list calculate_code make_proc prune_structured);

use strict;
use warnings;
use VCG;
use Set::IntRange;

use vars qw(@nodes @edges);

no warnings "recursion";

sub EVAL($);
sub compress($);
sub EVAL2($);
sub calculate_level_sub($$$$$);
sub get_ref($$$);


$::S_Max_Box_Lines1 	= 8;	# Max number of lines in a label (start)
$::S_Max_Box_Lines2 	= 2;	# Max number of lines in a label (end)
$::S_Max_Box_Chars 	= 25;	# Max number of chars/line in a box label
$::S_Max_Rhomb_Chars 	= 20;	# Max number of chars/line in a rhomb label

my $debug = 1;

# Takes a filename (or "-" for STDIN) and reads in a basic block file
# (the output of "basic_block foo.wsl" or of the @Basic_Blocks MetaWSL proc)
# Builds an array of hashes with all the information and returns
# a reference to the array, plus the start end end nodes:

sub bb_read($) {
  my ($input) = @_;
  if ($input eq "-") {
    open(IN, "<&STDIN") or die "Can't dup STDIN: $!\n";
  } else {
    open(IN, $input) or die "Can't read `$input': $!\n";
  }

  my @bb = ();

  my ($from, $posn, $len, $to, $code, $type, $n);

  my $entry = -1;

  # Data looks like this:
  # Node: 1 posn:(4) len: 3 --> (0)

  $bb[0] = { type => "END", succs_n => [], code => "end", num => 0 };

  while (<IN>) {
    next if (/^#/);
    next if (/^-->/ || /^<--/); # Skip debugging lines
    if (/^Entry node is: (\d+)$/) {
      $entry = $1;
      next;
    }
    next unless (($from, $posn, $len, $to)
		   = /^Node: (\d+) posn:\s*(\(.*?\)) len: (\d+) --> \((.*?)\)$/);

    # We take references to these, so they must be renewed on each iteration:
    my @succslist = split(/ /, $to);
    my @links = ();
    my @control = ();
    my %assigns = ();
    my %phi = ();
    print STDERR "$from -> " . join(",", @succslist) . "\n" if $debug > 2;
    $code = $_;
    $type = "block";
    while (<IN>) {
      last if (/^\s*$/);
      $code .= $_;
      chomp;
      if (/^\d{8} /) {
	push(@links, $_);
      } elsif (/^\d+: <(.*?)>$/) {
	@control = split(/,\s*/, $1);
      } elsif (/^\s+(\S+) := phi\((.*?)\)$/) {
	$phi{$1} = [split(/,\s*/, $2)];
      } elsif (/^\s+(\S+) := <(.*?)>$/) {
	$assigns{$1} = [split(/,\s*/, $2)];
      } else {
	$type = $_;
      }
    }
    chomp($code);

    # Add a hashref to the @bb array:
    $bb[$from] = { type => $type, posn => $posn, len => $len, succs_n => \@succslist,
		   links => \@links, control => \@control,
		   phi => \%phi, assigns => \%assigns,
		   code => $code, num => $from };
  }
  close(IN);
  die "No Entry node found in input!\n" if ($entry == -1);

  # Add the start node (no. 1) with an edge to the end node:
  $bb[1] = { type => "START", succs_n => [0, $entry], code => "start", num => 1 };
  print STDERR "1 -> $entry\n" if $debug > 2;

  # Ensure that each succs_n node exists:
  foreach $n (@bb) {
    next unless defined($n);
    foreach $to (@{$n->{succs_n}}) {
      next if defined($bb[$to]);
      print STDERR "Target node of $n->{num} -> $to does not exist!\n";
    }
  }

  # Now that we have the whole array of nodes, compute a succs entry
  # for each node containing a list of the successors (hashrefs):
  foreach $n (@bb) {
    next unless defined($n);
    $n->{succs} = [ map { $bb[$_] } @{$n->{succs_n}} ];
  }

  return(\@bb, $bb[1], $bb[0]);
}


sub comma(@) {
  join(", ", @_);
}


# Takes a basic block hash array and a filename (or "-" for STDOUT)
# and writes a bb file.

sub bb_write($$) {
  my ($bb, $output) = @_;
  if ($output eq "-") {
    open(OUT, ">&STDOUT") or die "Can't dup STDOUT: $!\n";
  } else {
    open(OUT, ">$output") or die "Can't write to `$output': $!\n";
  }
  my $entry = 1; # Entry node number
  foreach my $n (@$bb) {
    next unless defined($n);
    next if (($n->{num} == 0) || ($n->{num} == 1));
    print OUT "Node: $n->{num} posn:$n->{posn} len: $n->{len} --> ";
    print OUT "(", join(" ", map { $_->{num} } @{$n->{succs}}), ")\n";
    print OUT "$n->{type}\n" if ($n->{type} ne "block");
    foreach my $line (@{$n->{links}}) {
      print OUT "$line\n";
    }
    print OUT "0: <", comma(@{$n->{control}}), ">\n";
    foreach my $v (sort keys %{$n->{phi}}) {
      print OUT "   $v := phi(", comma(@{$n->{phi}->{$v}}), ")\n";
    }
    foreach my $v (sort keys %{$n->{assigns}}) {
      print OUT "   $v := <", comma(@{$n->{assigns}->{$v}}), ">\n";
    }
    print OUT "\n";
  }
  # Entry node is the successor to 1 (start) other than 0 (end):
  foreach my $n (@{$$bb[1]->{succs}}) {
    print OUT "Entry node is: $n->{num}\n" if ($n->{num} != 0);
  }
}



# Takes a reference to an array of basic block nodes
# and a hash table of options (defaults are in parentheses):
# output => output file name ("-" for STDOUT)
# orientation => "left_to_right" or "top_to_bottom" ("top_to_bottom")
# edges => $bb[$n]{edges} is the set of edges (succs)
# types => hash table of edge types ({})
# reverse => set to 1 to reverse all edges
# layout => layout algorithm, normal, maxdepth, mindepth, dfs, ... (maxdepth)

# The types hash table may contain an edge type:
# $types{$from,$to} = "edge", "nearedge", "backedge", ...

# Writes a VCG callgraph representing the graph of blocks.

sub bb_callgraph($$) {
  my ($bb, $opt) = @_;
  die "bb_callgraph requires an array reference for the nodes\n"
    unless (ref $bb eq "ARRAY");
  die "bb_callgraph requires an hash reference for the options\n"
    unless (ref $opt eq "HASH");

  my $output = $$opt{output} || "-";
  my $orientation = $$opt{orientation} || "top_to_bottom";
  my $edges = $$opt{edges} || "succs";
  my $edge_type = $$opt{types} || {};
  my $reverse = $$opt{reverse};

  my $saveout;
  local *OUT;
  if ($output ne "-") {
    open(OUT, ">$output") or die "Can't write to `$output': $!\n";
    $saveout = select(OUT);
  }

  # Set up @nodes and @edges:
  @nodes = ();
  @edges = ();
  my ($n, $shape, $type, $from, $to);
  my %targets = ();
  my %seen = ();

  foreach $n (@$bb) {
    next unless defined ($n);
    $type = $n->{type};
    print STDERR "$n: ", Dumper($n) unless defined($type);
    $seen{$n->{num}}++;

    if (($type eq "START") || ($type eq "END")) {
      $shape = "ellipse";
    } elsif ($type eq "IF") {
      $shape = "box";
    } else {
      $shape = "box";
    }
    $from = $n->{num};
    node($n->{num}, $n->{code}, $shape);
    foreach $to (map { $_->{num} } set_to_list($n->{$edges})) {
      $targets{$to}++;
      if ($reverse) {
	edge($to, $from, $$edge_type{$from,$to});
      } else {
	edge($from, $to, $$edge_type{$from,$to});
      }
    } ;
  }

  # If a target of an edge doesn't exist then xvcg will crash.
  # Put this here so that such a bug will create a ??? node instead:

  foreach $n (keys %targets) {
    node($n, "???", "ellipse") unless $seen{$n};
  }

  my $layout = "minbackward";	# Was maxdepth
  if ($$opt{layout}) {
    $layout = $$opt{layout};
  } elsif (($edges eq "idom") || ($edges eq "ipdom")) {
    $layout = "tree";
  }

  print_callgraph($layout, $orientation, \@nodes, \@edges);

  if ($output ne "-") {
    close(OUT);
    select($saveout);
  }
}



sub node($$$) {
  my ($title, $contents, $shape) = @_;
  my $label = make_label($shape, $contents, "");
  push(@nodes, [$shape, $title, $label, "", "", $contents, "", ""]);
}


sub edge($$$) {
  my ($from, $to, $type) = @_;
  $type ||= "edge";
  my ($col, $pri);
  if ($type eq "edge") {
    $col = "";
    $pri = "";
  } else {
    $col = "red";
    $pri = "0";
  }
  push(@edges, [$type, $from, $to, "", $pri, "", "", 2, $col]);
}


# The nodes list is a list of array refs:
# [shape, title, label, WSL, comments, code, edge_label, colour]
#
# The edges list is also a list of array refs:
# [type, source, target, label, pri, class, style, thick, colour]
#



### General purpose block manipulation routines:

# Calculate the reverse of a set of edges
# (eg calculate preds from succs):

sub calculate_reverse($$$) {
  my ($bb, $succs, $preds) = @_;
  my ($n, $to);

  foreach $n (@$bb) {
    next unless defined ($n);
    $n->{$preds} = [];
  }
  foreach $n (@$bb) {
    next unless defined ($n) && defined($n->{$succs});
    foreach $to (set_to_list($n->{$succs})) {
      push (@{$to->{$preds}}, $n);
    }
  }
}


# Compute level numbers:

sub calculate_level($$$$) {
  my ($bb, $start, $edge, $level) = @_;
  calculate_level_sub($bb, $start, $edge, $level, 0);
}

sub calculate_level_sub($$$$$) {
  my ($bb, $x, $edge, $level, $n) = @_;
  $x->{$level} = $n;
  $n++;
  foreach my $child (@{$x->{$edge}}) {
    calculate_level_sub($bb, $child, $edge, $level, $n)
      unless defined($child->{$level});
  }
}



# Dominator tree calculation.
# See ~/papers/SSA-PDG/dominators-paper.ps.gz ("Dominators in Linear Time",
# Alstrup, Harel, Lauridsen and Thorup)


# Calculate the set of dominators $D[n] for each node, ie the nodes
# which dominate n (appear on every path from 1 to n).

sub calculate_dom($$$$) {
  my ($bb, $start, $succs, $preds) = @_;
  calculate_reverse($bb, "succs", "preds") unless defined($start->{preds});
  my $BFS_node_list = breadth_first_list($start, $succs, "bfs_num");
  my ($n, $nn, $change, $newD, $p);
  my (@preds_n);
  my @D = ();
  my $node_list = Set::IntRange->new(0, $#$bb);
  my $start_n = $start->{num};
  # Only the entry node appears on every path from entry to entry:
  $node_list += $start_n;
  $D[$start_n] = $node_list->Clone();
  # Initially assume that all other nodes are dominated by everything:
  $node_list->Fill();
  foreach $n (0..$#$bb) { $D[$n] ||= $node_list->Clone() };
  $change = 1;
  while ($change) {
    $change = 0;
    foreach $n (@$BFS_node_list) {
      $nn = $n->{num};
      # skip entry node:
      next if ($nn == 1);
      # add the nodes in $D[$p] (for every pred $p of $n) to $newD:
      @preds_n = map { $_->{num} } @{$n->{$preds}};
      $newD = $D[shift(@preds_n)]->Clone();
      foreach $p (@preds_n) {
	# remove from $newD the elements not in $D[$p]
	$newD &= $D[$p];
      }
      $newD += $nn;
      if ($newD ne $D[$nn]) {
	$D[$nn] = $newD;
	$change = 1;
      }
    } # next $n in BFS order
  } # next while($change) loop
  foreach $nn (0..$#$bb) {
    next unless defined($$bb[$nn]);
    print STDERR "D[$nn] = $D[$nn]\n" if ($debug > 3);
    $$bb[$nn]{dom} = $D[$nn];
  }
}


# Return a reference to a list of the nodes in breadth first
# search order of the given type of edges:

sub breadth_first_list($$$) {
  my ($root, $edges, $bfs_num) = @_;
  my (@todo) = ($root);
  my (@done) = ();
  my $x;
  my @list = ();
  while (@todo) {
    $x = shift(@todo);
    next if ($done[$x->{num}]);
    $done[$x->{num}] = 1;
    push(@list, $x);
    $x->{$bfs_num} = $#list;
    # push the successors $x onto @todo:
    push(@todo, grep { !$done[$_->{num}] } set_to_list($x->{$edges}));
  }
  print STDERR "BFS Order: ", join(" ", map { $_->{num} } @list), "\n" if ($debug > 1);
  return(\@list);
}

# As above, but a depth first search:

sub depth_first_list($$$$) {
  my ($root, $edges, $dfs_num, $dfsp) = @_;
  $root->{$dfsp} = [];
  my (@todo) = ($root);
  my (@done) = ();
  my ($x, $y);
  my @list = ();
  while (@todo) {
    $x = pop(@todo);
    next if ($done[$x->{num}]);
    $done[$x->{num}] = 1;
    push(@list, $x);
    $x->{$dfs_num} = $#list;
    # push the successors of $x onto @todo:
    foreach $y (reverse grep { !$done[$_->{num}] } set_to_list($x->{$edges})) {
      $y->{$dfsp} = $x;
      push(@todo, $y);
    }
  }
  print STDERR "DFS Order: ", join(" ", map { $_->{num} } @list), "\n" if ($debug > 1);
  return(\@list);
}


# The immediate dominator is the first dominator found on
# any path from $n to the start (excluding $n).
# idom exists and is unique for any node except start,
# and the idom links form a tree.

sub calculate_idom($$$$$) {
  my ($bb, $start, $succs, $preds, $idom) = @_;
  calculate_dom($bb, $start, $succs, $preds) unless defined($start->{dom});
  # Do a breadth-first search, for each new node,
  # scan back up the path for the nearest dominator:
  my @todo = ([$start->{num}]);
  $start->{$idom} = [];
  my @done = ();
  my ($path, @path, $x, $dom, $y);
  while (@todo) {
    $path = shift(@todo);
    $x = $$path[0];
    next if ($done[$x]);
    $done[$x]++;
    # Search back up the path for a dominator:
    @path = @$path;
    $dom = $$bb[$x]{dom};
    for (;;) {
      shift(@path);
      last unless (@path);
      if ($dom->contains($path[0])) {
	$$bb[$x]{$idom} = $$bb[$path[0]];
	print STDERR "$idom", "{$x} = $path[0]\n" if ($debug > 1);
	last;
      }
    }
    # Add successors of $x to @todo:
    foreach $y (map { $_->{num} } @{$$bb[$x]{$succs}}) {
      next if ($done[$y]);
      push(@todo, [$y, @$path]);
    }
  }
}

# Find nodes which are unreachable from the given node
# via the given edges:

sub find_unreachable($$$) {
  my ($bb, $root, $edges) = @_;
  my @unreachable = ();
  my %done = ();
  my @todo = ($root);
  $done{$root->{num}}++;
  my ($x, $y, $n, $i);
  while (@todo) {
    $x = pop(@todo);
    push(@todo, grep { !$done{$_->{num}}++ } set_to_list($x->{$edges}));
  }
  foreach $i (0..$#$bb) {
    next unless defined($$bb[$i]);
    next if ($done{$i});
    push(@unreachable, $$bb[$i]);
  }
  return(\@unreachable);
}

# Delete nodes which are unreachable from the given node
# via the given edges.  Assumes $$bb[$i]->{num} = $i
# Shrinks the array and fixes {num} values to match:

sub delete_unreachable($$$) {
  my ($bb, $root, $edges) = @_;
  my %done = ();
  my @todo = ($root);
  $done{$root->{num}}++;
  my ($x, $y, $n, $i);
  while (@todo) {
    $x = pop(@todo);
    push(@todo, grep { !$done{$_->{num}}++ } set_to_list($x->{$edges}));
  }
  my @new = ();
  foreach $i (0..$#$bb) {
    next unless defined($$bb[$i]);
    if ($done{$i}) {
      $$bb[$i]->{num} = @new;
      push(@new, $$bb[$i]);
    } else {
      print STDERR "deleting $i\n" if ($debug > 1);
    }
  }
  # copy the new array to the referenced array:
  @$bb = @new;
}


sub set_to_list($) {
  my ($set) = @_;
  my $ref = ref($set);
  if ($ref eq "Set::IntRange") {
    return($set->to_List())
  } elsif ($ref eq "ARRAY") {
    return(@$set);
  } elsif ($ref eq "HASH") {
    return($set);
  } elsif (!$ref && defined($set)) {
    return($set);
  } else {
    confess "Argument of set_to_list is neither a set/array/hash ref nor a number\n"
      . Dumper($set) . "\n";
  }
}


# Find edges where the head dominates the tail,
# ie edges m --> n where n is in Dom(m)

sub calculate_back_edges($$) {
  my ($bb, $edge_type) = @_;
  calculate_dom($bb, $$bb[1], "succs", "preds") unless defined($$bb[1]{dom});
  my ($n, $m);
  for $n (@$bb) {
    next unless defined($n);
    for $m (@{$n->{preds}}) {
      if ($m->{dom}->contains($n->{num})) {
	print STDERR "Back edge: $m->{num} --> $n->{num}\n" if ($debug > 0);
	$$edge_type{$m->{num},$n->{num}} = "backedge";
      }
    }
  }
}


# Find "retreating" edges (edges which cause the topsort to fail)
# This assumes that back edges have already been found:

sub calculate_retreating_edges($$) {
  my ($bb, $edge_type) = @_;
  my @mins = ();     # List of minimal elements in @todo
  my @numpreds = (); # No. of predecessors of each element (ignoring $done[] elts)
  my ($elt, $pred, $minpreds, $minelt, $succ);
  my @done = ();
  my $count = 0;
  foreach $elt (@$bb) {
    next unless defined($elt);
    $count++;
    # Don't count backedges:
    foreach $pred (@{$elt->{preds}}) {
      $numpreds[$elt->{num}]++ unless ($$edge_type{$pred->{num},$elt->{num}});
    }
    push(@mins, $elt) if (!$numpreds[$elt]);
  }
  for (1..$count) {
    # Pick an element with the smallest number of predecessors
    # and remove it. Any edges to this element are retreating edges:
    if (@mins) {
      $minelt = shift(@mins);
    } else {
      # There must be a retreating edge since all elts have predecessors.
      # Find an element with the smallest number of predecessors:
      $minpreds = $count + 1; # Larger than any number of predecessors
      for $elt (@$bb) {
	next unless defined($elt);
	next if ($done[$elt->{num}]);
	if ($numpreds[$elt->{num}] < $minpreds) {
	  $minelt = $elt;
	  $minpreds = $numpreds[$elt->{num}];
	  last if ($minpreds == 1);
	}
      }
      # Edges from unprocessed elts to $minelt are back edges:
      for $elt (@{$minelt->{preds}}) {
	next if ($done[$elt->{num}]);
	next if ($$edge_type{$elt->{num},$minelt->{num}});
	print STDERR "Retreating edge: $elt->{num} --> $minelt->{num}\n" if ($debug > 0);
	$$edge_type{$elt->{num},$minelt->{num}} = "backedge";
      }
      $numpreds[$minelt->{num}] = 0;
    }
    # Decrement $numpreds[] for each successor of $minelt
    # and add to @mins if an element now has no predecessors:
    foreach $succ (@{$minelt->{succs}}) {
      $numpreds[$succ->{num}]--;
      push(@mins, $succ) if ($numpreds[$succ->{num}] == 0);
    }
    $done[$minelt->{num}]++;
    #push(@topsort, $minelt);
  }
}


# Calculate semidominators and dominators:

# sdom(b1) is the smallest numbered block b2 such that there is a control
# flow path from b2 to b1 in which all the intervening blocks have
# a larger DFS index than b1.

# Equivalently, sdom(b) is the smallest element of the union of:
# (1) The control flow predecessors of b with smaller dfs index and;
# (2) The sdoms of blocks which have dfs index greater than b and
#     have a path in the dfs tree to one of b's predecessors

# Set (2) can be calculated by finding the minimum sdom along the path
# in the dfs tree from b's larger numbered predecessors to the roots
# of those predecessors in the forest.

# NOTE:
# sdom is a DFS order number,
# path_min is a node hashref,
# bucket is an array of node numbers
# ancestor is a node number
# dfsp is a node hashref.

sub calculate_idom_fast($$$$$) {
  my ($bb, $start, $succs, $preds, $idom) = @_;
  calculate_reverse($bb, "succs", "preds") unless defined($start->{preds});
  my $dfs = depth_first_list($start, $succs, "dfs_num", "dfsp");

  my ($x, $succ);
  foreach $x (@$dfs) {
    # Initialise the sdom value to the dfs number:
    $x->{sdom} = $x->{dfs_num};
    $x->{path_min} = $x;
    $x->{bucket} = [];
    $x->{ancestor} = -1;
  }
  # $block->{ancestor} encapsulates the forest of trees from the DFS tree.
  # ancestor = -1 corresponds to a block which is a root of a tree in the forest.
  # When a block is processed we set ancestor to dfsp (the DFS parent).
  # path_min stores the minimum sdom value along all the paths in the forest.
  # (Lengauer and Tarjen call this "label").

  my ($w, $v, $u);
  foreach $w (reverse @$dfs[1..$#$dfs]) {
    foreach $v (@{$w->{$preds}}) {
      $u = EVAL($v);
      if ($u->{sdom} < $w->{sdom}) {
	$w->{sdom} = $u->{sdom};
      }
    }
    push(@{$$dfs[$w->{sdom}]->{bucket}}, $w);

    $w->{ancestor} = $w->{dfsp};
    foreach $v (@{$w->{dfsp}{bucket}}) {
      $u = EVAL($v);

      # Lengauer and Tarjan's code:
      # $v->{dom} = ($u->{sdom} < $v->{sdom}) ?
      #                 $u->{dfs_num}
      #               : $w->{dfsp}->{dfs_num};

      # Preston Briggs' code:
      $v->{dom} = ($v->{sdom} == $u->{sdom}) ?
		      $v->{sdom}
		    : $u->{dfs_num};
    }
    undef $w->{dfsp}{bucket};
  }

  # Now we can fix up the dominators.
  # If the dom value is the same as the sdom value, then it is correct.
  # Otherwise, the dominator is the dominator of the recorded value
  # (the dom of the recorded value will be correct by the time
  # we get to this element):

  my $i;
  foreach $i (@$dfs[1..$#$dfs]) {
    if ($i->{dom} != $i->{sdom}) {
      $i->{dom} = $$dfs[$i->{dom}]{dom};
    }
  }

  # Finally, set the idom node from the dom dfs number:
  $start->{$idom} = [];
  $start->{sdom} = [];
  foreach $i (@$dfs[1..$#$dfs]) {
    $i->{$idom} = $$dfs[$i->{dom}];
    $i->{sdom} = $$dfs[$i->{sdom}];
  }

}


# Find the node with the minimum semidominator along a path in the DFS tree
# while compressing the path:

sub EVAL($) {
  my ($v) = @_;
  if ($v->{ancestor} >= 0) {
    compress($v);
  }
  return($v->{path_min});
}


sub compress($) {
  my ($v) = @_;
  my $parent = $v->{ancestor};
  if ($parent->{ancestor} >= 0) {
    compress($parent);
    if ($parent->{path_min}{sdom} < $v->{path_min}{sdom}) {
      $v->{path_min} = $parent->{path_min};
    }
    $v->{ancestor} = $parent->{ancestor};
  }
}



sub calculate_idom_fast2($$$$$) {
  my ($bb, $start, $succs, $preds, $idom) = @_;
  calculate_reverse($bb, "succs", "preds") unless defined($start->{preds});
  my $dfs = depth_first_list($start, $succs, "dfs_num", "dfsp");

  my ($x, $succ);
  foreach $x (@$dfs) {
    $x->{bucket} = [];
    undef $x->{ancestor};
    undef $x->{samedom};
  }
  # $block->{ancestor} encapsulates the forest of trees from the DFS tree.
  # When a block is processed we set ancestor to dfsp (the DFS parent).
  # best stores the node with the minimum sdom value
  # along all the paths in the forest.

  my ($n, $p, $s, $sp, $v, $y);
  foreach $n (reverse @$dfs[1..$#$dfs]) {
    $p = $n->{dfsp};
    $s = $p;
    foreach $v (@{$n->{$preds}}) {

      if (!defined($v->{dfs_num})) {
        print STDERR join(", ", map { $_->{num} } @$dfs), "\n";
        print STDERR "v->{num} = $v->{num}\n";
        print STDERR "Keys of v are: ", join(" ", sort keys %$v), "\n";
        die "Node $n->{num}'s predecessor does not have a dfs_num";
      }

      if (!defined($n->{dfs_num})) {
        print STDERR "Keys of n are: ", join(" ", sort keys %$n), "\n";
        die;
      }

      if ($v->{dfs_num} <= $n->{dfs_num}) {
	$sp = $v;
      } else {
	$sp = EVAL2($v)->{sdom};
      }
      if ($sp->{dfs_num} <= $s->{dfs_num}) {
	$s = $sp;
      }
    }
    $n->{sdom} = $s;
    push(@{$s->{bucket}}, $n);

    $n->{ancestor} = $p;
    $n->{best} = $n;
    foreach $v (@{$p->{bucket}}) {
      $y = EVAL2($v);
      if ($y->{sdom}{num} == $v->{sdom}{num}) {
	$v->{$idom} = $p;
      } else {
	$v->{samedom} = $y;
      }
    }
    undef $p->{bucket};
  }
  foreach $n (@$dfs[1..$#$dfs]) {
    if (defined($n->{samedom})) {
      $n->{$idom} = $n->{samedom}{$idom};
    }
  }
  $start->{$idom} = [];
  $start->{sdom}  = [];
}


# Find the ancestor with smallest sdom dfs number,
# using path compression for efficiency:

sub EVAL2($) {
  my ($v) = @_;
  my ($a, $b);
  $a = $v->{ancestor};
  if (defined($a->{ancestor})) {
    $b = EVAL2($a);
    $v->{ancestor} = $a->{ancestor};
    if ($b->{sdom}{dfs_num} < $v->{best}{sdom}{dfs_num}) {
      $v->{best} = $b;
    }
  }
  return ($v->{best});
}


# Recompute code from control, phi and assigns:

sub calculate_code($) {
  my ($bb) = @_;
  foreach my $n (@$bb) {
    next unless (defined($n->{control}));
    $n->{code} =~ s/(\n\d+:).*$/$1/s;
    $n->{code} .= " <" . join(", ", @{$n->{control}}) . ">";
    foreach my $var (sort keys %{$n->{phi}}) {
      $n->{code} .= "\n   $var := phi(" . join(", ", @{$n->{phi}->{$var}}) . ")";
    }
    foreach my $var (sort keys %{$n->{assigns}}) {
      $n->{code} .= "\n   $var := <" . join(", ", @{$n->{assigns}->{$var}}) . ">";
    }
  }
}


# Create a new node and add to the array:

sub new_node($$) {
  my ($bb, $type) = @_;
  my $n = {};
  $n->{type} = $type;
  $n->{posn} = "()";
  $n->{len} = 0;
  $n->{control} = [];
  $n->{assigns} = {};
  $n->{num} = @$bb;
  push(@$bb, $n);
  return($n);
}


# Make a new procedure from the given blocks and parameter lists
# Link it to an existing WHERE Header or create a new header node.
# $bb, $enter, $exit, $body, $val and $var are array refs

sub make_proc($$$$$$$$$) {
  my ($bb, $start, $end, $name, $enter, $exit, $body, $val, $var) = @_;
  # Delete any links from outside the body to inside the body
  # and vice-versa
  my %body = ();
  foreach (@$body) { $body{$_}++ };
  foreach my $n (@$bb) {
    if ($body{$n}) {
      if (grep { !$body{$_} } @{$n->{succs}}) {
	$n->{succs} = [grep { $body{$_} } @{$n->{succs}}];
      }
    } else {
      if (grep { $body{$_} } @{$n->{succs}}) {
	$n->{succs} = [grep { !$body{$_} } @{$n->{succs}}];
      }
    }
  }
  # Set up header and return nodes:
  my $header = new_node($bb, "PROC Header $name");
  $header->{succs} = [@$enter];
  foreach my $v (@$val) {
    $header->{assigns}->{$v} = [];
  }
  my $return = new_node($bb, "PROC Return $name");
  $return->{succs} = [$end];
  foreach my $v (@$var) {
    $return->{assigns}->{$v} = [$v];
  }
  foreach my $n (@$exit) {
    # Remove $end from succs and add $return
    $n->{succs} = [grep { $_ ne $end } @{$n->{succs}}];
    push(@{$n->{succs}}, $return);
  }
  # Look for or create a WHERE Header:
  my $where;
  foreach my $n (@$bb) {
    if ($n->{type} eq "WHERE Header") {
      $where = $n;
      last;
    }
  }
  if (!defined($where)) {
    $where = new_node($bb, "WHERE Header");
    $where->{succs} = $start->{succs};
    $start->{succs} = [$where];
  }
  push(@{$where->{succs}}, $header);
}


# Recursively replace structured subcomponents by a single node:
# the result is the remaining unstructured components
# from which we can compute the McCabe Essential Complexity.

# Sequence:    A -> B,  #preds(B) = 1, delete B
# Repeat loop: A -> (A, B), delete recursion in A and node B
# IF/CASE:     A -> (B1, ..., Bn) -> C, #preds(C) = n, delete B1, ..., Bn, C
# While loop:  A -> (B, C) and B -> A, #precs(C) = 1, delete B and C

sub prune_structured($$$) {
  my ($bb, $start, $V) = @_;

  calculate_reverse($bb, "succs", "preds");

  my $done;
  for (;;) {
    $done = 1;
    foreach my $n (@$bb) {
      next unless defined($n);
      next if $n->{num} == $start->{num}; 
      my @s = set_to_list($n->{succs});
      # Remove duplicates in @s (due to removing sequences in conditional branches):
      if (@s > 1) {
	my %done = ();
	my @new = ();
	foreach my $s (@s) {
	  if (!$done{$s->{num}}++) {
	    push(@new, $s);
	  }
	}
	if (@new != @s) {
          $done = 0;
	  @s = @new;
	  $n->{succs} = \@new;
	  # Check for dups in preds of @s:
	  foreach my $s (@s) {
	    my @p = ();
	    my %done = ();
	    foreach my $p (set_to_list($s->{preds})) {
	      if (!$done{$p->{num}}++) {
		push(@p, $p)
	      }
	    }
            if (set_to_list($s->{preds}) != @p) {
	      print "Case: $n->{num} -> $p[0]->{num}\n" if $V;
  	      $s->{preds} = \@p;
	    }
	  }
	}
      }
      # If $n calls itself, then remove the edge:
      if (grep { $_->{num} == $n->{num} } @s) {
	print "While loop: $n->{num} -> $n->{num}\n" if $V;
	$done = 0;
	$n->{succs} = delete_elt($n->{succs}, $n);
	$n->{preds} = delete_elt($n->{preds}, $n);
	next;
      }
      if (@s == 1) {
        my $succ = $s[0];
	my @p = set_to_list($n->{preds});
        if (@p == 1) {
	  my $p = $p[0];
          next if $p->{num} == $start->{num};
	  print "Sequence: $p->{num} -> $n->{num} -> $succ->{num}\n" if $V;
          # Have found a sequence: $p -> $n -> $succ
          # Delete node $n:
	  $done = 0;
	  # Replace $n in $p->{succs} by $succ:
	  my @new = ();
	  foreach my $s (set_to_list($p->{succs})) {
	    if ($s->{num} == $n->{num}) {
	      push(@new, $succ);
	    } else {
	      push(@new, $s);
	    }
	  }
	  $p->{succs} = \@new;
	  delete_node($bb, $n, $p);
	  next;
	} elsif ((@p == 1) && ($p[0]->{num} == $succ->{num})) {
          # Node n has one predecessor and one sucessor which are the same node:
          # so this is a while loop:
	  $done = 0;
          print "While loop: $succ->{num} -> $n->{num}\n" if $V;
          # Remove $n from $pred's succs and preds and delete $n:
          $succ->{succs} = delete_elt($succ->{succs}, $n);
          $succ->{preds} = delete_elt($succ->{preds}, $n);
          $$bb[$n->{num}] = undef;
	  next;
	} elsif ((@p == 2) && grep { $_->{num} == $succ->{num} } @p) {
          # Node n has one sucessor and two predecessors,
	  # one of which is its successor:
          # so this is a repeat loop:
	  $done = 0;
          print "Repeat loop: $n->{num} -> $succ->{num}\n" if $V;
	  $n->{succs} = delete_elt($succ->{succs}, $n);
	  $n->{preds} = delete_elt($n->{preds}, $succ);
	  delete_node($bb, $succ, $n);
	  next;
	}
      } elsif ((@s == 2) && grep { $_->{num} == $n->{num} } @s) {
	my $succ = ($s[0]->{num} == $n->{num}) ? $s[1] : $s[0];
	if (set_to_list($succ->{preds}) == 1) {
	  print "Repeat loop: $n->{num} -> $succ->{num}\n" if $V;
	  # Delete the $succ node
	  $done = 0;
	  $n->{succs} = $succ->{succs};
	  delete_node($bb, $succ, $n);
	  next;
	}
      }
      if (@s > 1) {
	# CASE statement:
	# Check that all succs have the same (single) succ and one pred:
	my $ok = 1;
	my $end = undef;
	foreach my $succ (@s) {
	  if ((set_to_list($succ->{preds}) != 1)
	        || (set_to_list($succ->{succs}) != 1)) {
	    $ok = 0;
	    last;
	  } elsif (!defined($end)) {
	    $end = (set_to_list($succ->{succs}))[0];
	  } elsif ((set_to_list($succ->{succs}))[0]->{num} != $end->{num}) {
	    $ok = 0;
	    last;
	  }
	}
	if ($ok) {
	  $done = 0;
	  print "Case: $n->{num} -> (", join(",", map { $_->{num} } @s), ")\n" if $V;
	  foreach my $succ (@s) {
	    $$bb[$succ->{num}] = undef;
	  }
	  $n->{succs} = [$end];
	  # Remove all of @s from $end's predecessors and add $n instead:
	  my @new = ($n);
	  foreach my $p (set_to_list($end->{preds})) {
	    if (grep { $_->{num} == $p->{num} } @s) {
	      # Remove this pred
	    } else {
	      push(@new, $p);
	    }
	  }
	  $end->{preds} = \@new;
	}
      }
    }
    last if $done;
  }
  print "\n" if $V;
  # Add a node if necessary:
  my $count = 0;
  foreach my $n (@$bb) {
    next unless defined($n);
    $count++;
    print "$n->{num} " if $V;
  }
  print "\n" if $V;
  print "count = $count\n" if $V;
  if ($count < 3) {
    my $n = new_node($bb, "");
    $$bb[1]->{succs} = [$n];
    $n->{succs} = [$$bb[0]];
    $$bb[0]->{preds} = [$n];
    $n->{preds} = [$$bb[1]];
  }
}


# Delete node $succ and fix preds for its successors
# to point to $n instead of succ

sub delete_node($$$) {
  my ($bb, $succ, $n) = @_;
  foreach my $m (set_to_list($succ->{succs})) {
    # Replace each successor by $n in preds list for $m
    my @new = ();
    foreach my $p (set_to_list($m->{preds})) {
      if ($p->{num} == $succ->{num}) {
        push(@new, $n);
      } else {
        push(@new, $p);
      }
    }
    $m->{preds} = \@new;
  }
  $$bb[$succ->{num}] = undef;

  #print "Delete node $succ->{num}\n", join(",", caller), "\n";
  #bb_write($bb, "tmp.bb");
  #system "bbtovcg tmp.bb";
  #system "xvcg tmp.cfg";

}


# Delete an element from a list/set of nodes:

sub delete_elt($$) {
  my ($l, $n) = @_;
  my @new = ();
  my $found = 0;
  foreach my $m (set_to_list($l)) {
    if ($m->{num} == $n->{num}) {
      $found++;
    } else {
      push(@new, $m);
    }
  }
  warn "Node $n->{num} not found in list!\n" . join(",", caller)
    unless $found == 1;
  return(\@new);
}



sub check_preds($) {
  my ($n) = @_;
  foreach my $p (set_to_list($n->{preds})) {
    die "check_preds: bad pred for $n->{num}: ", join(",", caller)
      unless ref($p) eq "HASH";
  }
}


1;

