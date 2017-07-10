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
# Control Dependance calculation
#
# See "Optimal Control Dependence Computation and the Roman Chariots Problem"
# K. Pingali and G. Bilardi (~/SSA-PDG/pingali-roman-chariots.ps.gz)
#

package Control_Dep;
require 5.000;
require Exporter;
use Carp;

@ISA = qw(Exporter);
@EXPORT = qw(construct_roman_chariots conds_preprocessing conds_query
	     cdequiv_preprocessing compute_conds cd_set);

use strict;
use warnings;
use Blocks;
use Set::IntRange;

sub conds_visit($$$);


# Additional keys for the hashes in the @$bb array:

# cd_bndry	 Record if this is a boundary node
# cd_next_bndry	 Link to the nearest ancestral boundary node
# cd_L		 For each leaf or boundary, list the set of routes through here
#		 (for a non-boundary node, just list the routes starting here)
# cdequiv_next   Next node in the list of cdequiv nodes
# cdequiv_header First node in the list of cdequiv nodes
# conds		 For cdequiv_header nodes: the list of nodes on which this node depends

# For slicing, if node $n is needed, then so are all the nodes
# in $n->{conds} (or $n->{cdequiv_header}->{conds})
# The latter nodes will all have multiple exits:
# typically they are "test" nodes with control variables but no assignments,
# but any assignments or phi assignments involving the control variables
# will also be needed.


# Construct a Roman Chariots problem for the given CFG:
# Input: a ref to an array of nodes and the end node.
# a hash table ref in which to record the index for each edge $u -> $v
# by setting $$edge_info{$u->{num},$v->{num}} = $i
# Output:
# (1) A ref to an array of cd sets, implemented as pairs of nodes
# where [$v, $p] represents the set [$v, $p),
#   (ie the set {$v, $v->{ipdom}, $v->{ipdom}->{ipdom}, ...}
#   up to, but not including, $p.
#   If $p = $v->{ipdom} then the set is empty and is not recorded);
# (2) A ref to an array of edges, where the ith cd set is the
# set of control dependencies for the ith edge;
# (3) A hash table which gives the index for each edge
# (where the edge is given as the pair of node numbers).
# 

sub construct_roman_chariots($$) {
  my ($bb, $end) = @_;

  # Calculate the postdominator tree:
  calculate_idom_fast2($bb, $end, "preds", "succs", "ipdom");
  
  # We want to walk the postdominator tree, top-down
  # so we to a BFS search of the reverse of ipdom:
  calculate_reverse($bb, "ipdom", "rev_ipdom");
  my $top_down = breadth_first_list($end, "rev_ipdom", "cd_bfs_num");

  my @A = ();	# The array of chariot routes
  my @E = ();	# The corresponding array of edges
  my %edge_index = ();	# The hash table which gives the index of each edge
  my $i = 0;	# index into @A and @E
  foreach my $p (@$top_down) {
    foreach my $u (@{$p->{rev_ipdom}}) {
      foreach my $v (@{$u->{succs}}) {
	if ($v->{num} != $p->{num}) {
	  # Append a cd set (a chariot route) to the end of A:
	  $A[$i] = [$v, $p];
	  # Note that the cd set of edge u->v is $i
	  $E[$i] = [$u, $v];
	  $edge_index{$u->{num},$v->{num}} = $i;
	  $i++;
	}
      }
    }
  }
  return(\@A, \@E, \%edge_index);
}


# Construct the APT structure
# Input: a ref to an array of nodes, the end node,
# $A = a ref to the array of chariot routes returned by construct_roman_chariots,
# and the real number constant $alpha
# Output: adds cd_bndry, cd_next_bndry, A and cd_L keys to the $bb array nodes.
# $v->{A} is the number of routes passing through $v, ie A_v
# $v->{b} is the number of routes with bottom node $v
# $v->{t} is the number of routes with top node $v
#
# This assumes that rev_ipdom is already available in $bb
# (since it is calculated in construct_roman_chariots)

sub conds_preprocessing($$$$) {
  my ($bb, $end, $A, $alpha) = @_;
  my $dfs = depth_first_list($end, "rev_ipdom", "cd_dfs_num", "cd_dfsp");
  foreach my $v (@$bb) {
    $v->{b} = 0;	# $b[v] = number of routes with bottom node v
    $v->{t} = 0;	# $t[v] = number of routes with top node v
  }
  foreach my $route (@$A) {
    $$route[0]->{b}++;
    $$route[1]->{t}++;
  }
  # Determine boundary nodes
  # $v->{A} is the size of the output for $v
  # $v->{z} is the zone size for $v
  # We want to walk the postdominator tree in both orders:
  my $top_down  = breadth_first_list($end, "rev_ipdom", "cd_bfs_num");
  my $bottom_up = [reverse(@$top_down)];
  foreach my $v (@$bottom_up) {
    # Compute the output size and tentative zone size
    # Output size = all incoming routes (from the children)
    # plus routes that start here minus routes that terminate here:
    $v->{A} = $v->{b} - $v->{t};
    $v->{z} = 1;
    my @children = set_to_list($v->{rev_ipdom});
    foreach my $u (@children) {
      $v->{A} += $u->{A};
      $v->{z} += $u->{z};
    }
    # All leaf nodes are boundary nodes,
    # otherwise make the zone as big as possible, but not bigger
    # than alpha * output_size(v) + 1
    if (!@children || ($v->{z} > $alpha * $v->{A} + 1)) {
      # Begin a new zone
      $v->{cd_bndry} = 1;
      $v->{z} = 1;
    } else {
      # Put $v in the same zone as its children:
      $v->{cd_bndry} = 0;
    }
  }
  # Chain each node to the first boundary node which is an ancestor,
  # so that when we generate the list of routes we can jump directly
  # to the right boundary (caching) nodes by following the chain:
  foreach my $v (@$top_down) {
    if ($v->{num} == $end->{num}) {
      # $v is the root of the postdominator tree
      $v->{cd_next_bndry} = 0;
    } elsif ($v->{ipdom}->{cd_bndry}) {
      $v->{cd_next_bndry} = $v->{ipdom};
    } else {
      $v->{cd_next_bndry} = $v->{ipdom}->{cd_next_bndry};
    }
  }
  # Add each route index in A to the relevant $b->{cd_L} for each boundary node $b
  foreach my $i (0..$#$A) {
    my $w = $$A[$i][0];
    my $t = $$A[$i][1];
    # while $t is a proper ancestor of $w...
    while ($t->{cd_bfs_num} < $w->{cd_bfs_num}) {
      push(@{$w->{cd_L}}, $i);
      $w = $w->{cd_next_bndry};
      last unless($w);
    }
  }
}


# For a given node $n, conds_query($n) returns the set (array ref)
# of edge indices (into the @E array of edges)
# which are the control dependencies of $n.
# For each edge (x, y) in the set, node y is postdominated by $n
# (ie every path from y to the end passes through $n) while node x
# has another outgoing edge which, if taken, may avoid $n.
# So if node $n is in the slice, then node x must be added to the slice.
# More precisely: if $n is in the slice and $i is in conds_query($n)
# then node $E[$i][0] is added to the slice, together with its control vars.

# Return the list of indexes in @A of chariot routes
# which contain the given node.
# Note that $E[$i] is the edge corresponding to chariot route
# (or cd set) $A[$i].

sub conds_query($$) {
  my ($query_node, $A) = @_;
  local @Control_Dep::output = ();
  conds_visit($query_node, $query_node, $A);
  return([@Control_Dep::output]);
}


sub conds_visit($$$) {
  my ($query_node, $visit_node, $A) = @_;
  foreach my $i (@{$visit_node->{cd_L}}) {
    if ($$A[$i][1]->{cd_dfs_num} < $query_node->{cd_dfs_num}) {
      push(@Control_Dep::output, $i);
    } else {
      last;
    }
  }
  if (!$visit_node->{cd_bndry}) {
    # Visit each child of visit_node (in the ipdom tree)
    foreach my $C (@{$visit_node->{rev_ipdom}}) {
      conds_visit($query_node, $C, $A);
    }
  }
}


# Control Dependance Equivalence Classes
# -- find the set of nodes with the same control dependence
# edges as the given node.
# This uses the fact that A_v (the cdequiv set) is characterised
# by |A_v| and Lo(A_v) (the least common ancestor of the bottom nodes)
# For sets with at least one element in common, (true for cdequiv sets),
# Lo(A_v) is the lowest common element.

# $A is a ref to the list of chariot routes
# returned by construct_roman_chariots

# Output: Adds cdequiv_next and cdequiv_header keys
# (also adds h_v, recent_size and recent_node keys which are used internally)

sub cdequiv_preprocessing($$$) {
  my ($bb, $end, $A) = @_;
  # Set up the extra $inf node, but don't add it to the list:
  my $inf = {};
  $inf->{recent_size} = 0;
  $inf->{recent_node} = $inf;
  $inf->{cdequiv_next} = undef;
  $inf->{cdequiv_header} = $inf;
  $inf->{S} = []; # Empty stack
  # $inf is a descendant of every node: 
  $inf->{cd_dfs_num} = @$bb;
  $inf->{num}        = @$bb;
  # Compute h_v = min { t | [v, t) \in A } for each node v
  map { $_->{h_v} = $inf } @$bb;
  foreach my $route (@$A) {
    my $v = $$route[0];
    my $t = $$route[1];
    $v->{h_v} = $t if ($t->{cd_dfs_num} < $v->{h_v}->{cd_dfs_num});
  }

  my $top_down  = depth_first_list($end, "rev_ipdom", "cd_dfs_num", "cd_dfsp");
  my $bottom_up = [reverse(@$top_down)];
  foreach my $v (@$bottom_up) {
    # Compute t_v
    # h_v = min { t | [v,t) is a chariot route }
    # Set h_below = min { H[C] | C is a child of v }
    # Set v1 to any child of v with value 
    my $h_below = $inf;
    my $v1 = $inf;
    foreach my $C (@{$v->{rev_ipdom}}) {
      if ($C->{H}->{cd_dfs_num} < $h_below->{cd_dfs_num}) {
	$h_below = $C->{H};
	$v1 = $C;
      }
    }
    # H[v] = min { h_v, h_below }
    $v->{H} = min($v->{h_v}, $h_below);
    # H_v2 = min { H[C] | C is any child of v other than v1 }
    my $H_v2 = $inf;
    foreach my $C (@{$v->{rev_ipdom}}) {
      if ($C->{cd_dfs_num} != $v1->{cd_dfs_num}) {
	if ($C->{H}->{cd_dfs_num} < $H_v2->{cd_dfs_num}) {
	  $H_v2 = $C->{H};
	}
      }
    }
    # t_v = min { h_v, H_v2, v }
    my $t_v = min($v->{h_v}, $H_v2, $v);

    # Copy the stack from v1 to v:
    if ($v1->{num} == $inf->{num}) {
      $v->{S} = [];
    } else {
      $v->{S} = $v1->{S};
    }

#print "$v->{num}: h_v = $v->{h_v}->{num}, h_below = $h_below->{num}, v1 = $v1->{num}, ";
#print "dfs = $v->{cd_dfs_num}, t_v = $t_v->{num}\n";

    # We don't need the stacks for the children now:
    foreach my $C (@{$v->{rev_ipdom}}) {
      delete $C->{S};
    }    

    # From S[v], pop all pairs [x, y) where y is a descendant of t_v
    while (@{$v->{S}}
	     && $v->{S}[$#{$v->{S}}][1]->{cd_dfs_num} >= $t_v->{cd_dfs_num}) {
      pop(@{$v->{S}});
    }

    # If [v, t_v) is not empty, then push [v, t_v) onto S[v]
    if ($v->{cd_dfs_num} > $t_v->{cd_dfs_num}) {
      push(@{$v->{S}}, [$v, $t_v]);
    }

    # Lo(v) is the first element of the top pair in S[v]
    # If S[v] is empty then there are no routes through v:
    my $Lo;
    if (@{$v->{S}}) {
      $Lo = $v->{S}[$#{$v->{S}}][0];
    } else {
      $Lo = $inf;
    }

#print "$v->{num}: stack = (", join(", ", map { "[$_->[0]->{num}, $_->[1]->{num}]" }
#                                             @{$v->{S}}), ")\n";
#print "$v->{num}: Lo = $Lo->{num}, Size = $v->{A}\n";

    # Determine the class of the node v:
    $v->{recent_size} = 0;
    $v->{recent_node} = $v;
    $v->{cdequiv_next} = undef;
#print "Lo->{recent_size} = $Lo->{recent_size}, v->{A} = $v->{A}\n";
    if ($Lo->{recent_size} == $v->{A}) {
      # Add $v to the current cdequiv class:
      $Lo->{recent_node}->{cdequiv_next} = $v;
      $v->{cdequiv_header} = $Lo->{recent_node}->{cdequiv_header};
    } else {
      # Start a new cdequiv class:
      $Lo->{recent_size} = $v->{A};
      $v->{cdequiv_header} = $v;
    }
    $Lo->{recent_node} = $v;
  }
}
  

# Return the node with the smallest DFS number:

sub min(@) {
  my $min = $_[0];
  foreach my $n (@_) {
    $min = $n if ($n->{cd_dfs_num} < $min->{cd_dfs_num});
  }
  return($min);
}
  


# After cdequiv_preprocessing we can use this to record
# the output of conds_query for cdequiv_header nodes:

sub compute_conds($$$) {
  my ($bb, $A, $E) = @_;
  foreach my $n (@$bb) {
    next unless ($n->{num} == $n->{cdequiv_header}->{num});
    my $conds = conds_query($n, $A);
    $n->{conds} = [map { $$E[$_][0] } @$conds];
  }
}


# For forward slicing we need the cd set of an edge:

sub cd_set($$) {
  my ($u, $v) = @_;
  my $p = $u->{ipdom};
  my $n = $v;
  my @cd = ();
  while ($n->{num} ne $p->{num}) {
    push(@cd, $n);
    $n = $n->{ipdom};
    die "Undefined ipdom for node $n->{num}!\n" unless defined($n);
  }
  return(\@cd);
}
  


1;
