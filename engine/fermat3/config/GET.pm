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
# Perl module to generate a GET file
#

package GET;

require 5.000;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(print_GET_flowchart read_GET_graph);

use strict;

# The nodes list is a list of array refs:
# [shape, title, label, WSL, comments, code, edge_label, colour]
#
# The edges list is also a list of array refs:
# [type, source, target, label, pri, class, style, thick, colour]
#

my %GET_node_shape = ();
#$GET_node_shape{box}		= "TSETextView";
$GET_node_shape{ellipse}	= "TSEShapeNodeView 0";
$GET_node_shape{parallelogram}	= "TSEShapeNodeView 1";
$GET_node_shape{rhomb}		= "TSEShapeNodeView 2";
$GET_node_shape{triangle}	= "TSEShapeNodeView 3";
$GET_node_shape{box}		= "TSEShapeNodeView 4";
$GET_node_shape{pentagon}	= "TSEShapeNodeView 5";
$GET_node_shape{hexagon}	= "TSEShapeNodeView 6";
$GET_node_shape{twobar1}	= "TSEShapeNodeView 7";
$GET_node_shape{twobar2}	= "TSEShapeNodeView 8";
$GET_node_shape{rounded}	= "TSEShapeNodeView 9";

# Convert VCG colours to GET style 0xbbggrr 24 bit number:

my %GET_colour = ();
$GET_colour{""} 	= "0xffffff";
$GET_colour{"14"}	= "0xff0000";
$GET_colour{"32"}	= "0xc0c0c0";


# Convert GET node shapes to VCG names:

my %VCG_node_shape = ();
$VCG_node_shape{"TSETextView"}        = "box";
$VCG_node_shape{"TSEShapeNodeView 0"} = "ellipse";
$VCG_node_shape{"TSEShapeNodeView 1"} = "parallelogram";
$VCG_node_shape{"TSEShapeNodeView 2"} = "rhomb";
$VCG_node_shape{"TSEShapeNodeView 3"} = "triangle";
$VCG_node_shape{"TSEShapeNodeView 4"} = "box";
$VCG_node_shape{"TSEShapeNodeView 5"} = "pentagon";
$VCG_node_shape{"TSEShapeNodeView 6"} = "hexagon";
$VCG_node_shape{"TSEShapeNodeView 7"} = "twobar1";
$VCG_node_shape{"TSEShapeNodeView 8"} = "twobar2";
$VCG_node_shape{"TSEShapeNodeView 9"} = "rounded";

# Convert GET colours to VCG numbers:

my %VCG_colour = ();
$VCG_colour{"0xffffff"} = "";
$VCG_colour{"0xff0000"}	= "17";
$VCG_colour{"0xc0c0c0"}	= "32";


# Print a flowchart in GET format (Graphics Editor Toolkit):

sub print_GET_flowchart($$$) {
  my ($basename, $nodes, $edges) = @_;
  my ($node, $edge);

  init_GET_flowchart($basename);
  print "// Nodes\n";
  foreach $node (@{$nodes}) {
    next unless ($node);
    GET_node(@{$node}) if ($node);
  }
  print "// Edges\n";
  foreach $edge (@{$edges}) {
    next unless ($edge);
    GET_edge(@{$edge});
  }
  end_GET_flowchart();
}


sub GET_node($$$$$$$$) {
  my ($shape, $title, $label, $wsl, $comments, $code, $l, $col) = @_;
  return() if (($shape eq "begin") || ($shape eq "end"));
  $label =~ s/ +(\|?$)/$1/mg;
  my ($shift, $width, $height) = node_size($shape, $label);
  $label = join("\n", map { (" " x $shift) . $_ } split(/\n/, $label));
  $label =~ s/\\"/"/g;
  $label =~ tr/\n/\~/;
  $label =~ s/\s+(\|?(\~|$))/$1/g;
  # Trim to 255 chars:
  substr($label, 252) = "" if (length($label) > 252);
  print <<END_NODE;

// Node
// name
"$title"
// height
$height
// width
$width
// Editor Node
Version 3.0
$label
$GET_node_shape{$shape}
0x0
$GET_colour{$col}
1
END_NODE
  # Currently there is nowhere to store the $wsl, $comments and $code strings
}


# Compute the size of a node, given the label text:
# Return (shift, width, height)
# width = 10*chars + 20
# height = 20*lines + 20
# shift = the number of spaces to add to the left column

sub node_size($$) {
  my ($shape, $label) = @_;
  my $chars = max(0, map { length($_) } split(/\n/, $label));
  my $lines = ($label =~ tr/\n/\n/) + 1;
  if ($shape eq "box") {
    return(0, 10 * $chars + 20, 20 * $lines + 20);
  } elsif ($shape eq "rhomb") {
    return(int($chars / 2 + 0.5), 20 * $chars + 10, 40 * $lines + 20);
  } elsif ($shape eq "ellipse") {
    return(1, 10 * $chars + 20, 20 * $lines + 20);
  } elsif ($shape eq "triangle") {    
    return(int($chars / 2 + 0.5), 20 * $chars + 10, 20 * $lines + 20);
  } elsif ($shape eq "twobar1") {
    return(3, 10 * $chars + 70, 20 * $lines + 20);
  } elsif ($shape eq "twobar2") {
    return(3, 10 * $chars + 70, 20 * $lines + 20);
  } else {

foreach my $i (0..999) {
  my ($package, $filename, $line, $subroutine) = caller($i);
  last unless defined($package);
  $filename =~ s!^.*/!!;
  print STDERR "filename = $filename, line = $line, subroutine = $subroutine\n";
}

    die "GET.pm: label_size: unknown shape: $shape\n";
  }
}


sub max(@) {
  my $max = $_[0];
  local $_;
  foreach (@_) {
    $max = $_ if ($_ > $max);
  }
  return($max);
}


sub GET_edge($$$$$$$$$) {
  my ($type, $source, $target, $label, $pri,
      $class, $style, $thick, $colour) = @_;
  print <<END_EDGE;
// Edge
// fromNodeName
"$source"
// toNodeName
"$target"
END_EDGE

  if ($label ne "") {
    print <<END_LABEL;
// Labels
Begin
// Label
// name
$label
// width
46
// height
19
// Editor Label
TSELabelView
0xfe000000
0x0
0
1
// Labels
End
// Editor Edge
TSEPropertyEdgeView
0x0 1 0 
END_LABEL

  }
}


sub init_GET_flowchart($) {
  my ($basename) = @_;
  $basename =~ tr/a-z/A-Z/;
  print <<END_OF_HEADER;

// Hierarchical Layout
// discardHLevels
TRUE
// disableMinimumSlopePercent
FALSE
// minimumSlopePercent
20
// occludeToCenters
TRUE
// properHierarchy
TRUE
// orthogonalRouting
TRUE
// variableLevelSpacing
FALSE
// orthogonalChannelMerging
FALSE
// orthogonalPortSharingMinimization
FALSE
// rowSpacing
12
// columnSpacing
12
// hlevelSpacing
40
// hlevelFramePercent
60
// hnodeSpacing
24
// hnodeFramePercent
60
// hlevelOrientation
TOP_TO_BOTTOM
// hlevelJustification
CENTER_JUSTIFIED
// stretchableSize
40
// staggerEndNodes
FALSE
// staggerEndNodesBy
20
// reduceCrossingsOnIncremental
TRUE
// magnifiedSpacing
FALSE
// digraphSpacing
40
// digraphFramePercent
5
// usePorts
FALSE
// incrementalLayout
FALSE
// positionLabelsOnLayout
TRUE
// labelPositioningQuality
5
// allowedLabelOverlapPercent
0
// randomize
FALSE
// undirected
FALSE
// occlude
TRUE
// disconnectedNodeMagnifiedRowSpacing
7
// disconnectedNodeMagnifiedColumnSpacing
7
// disconnectedNodeConstantRowSpacing
20
// disconnectedNodeConstantColumnSpacing
20
// disconnectedNodeLayoutStyle
DISCONNECTED_NODES_TILE
// disconnectedNodePackingStyle
DISCONNECTED_NODES_PACK_UPPER_LEFT
// disconnectedNodeTilingStyle
DISCONNECTED_NODES_TILE_TO_ROWS
// tileDisconnectedNodesByName
TRUE
// staggerDisconnectedNodes
FALSE
// staggerDisconnectedNodesBy
12
// generateDrawingLists
TRUE
// needsRecalculation
TRUE
// needsRelabeling
TRUE
// PostScript
// drawNodesBeforeEdges
TRUE
// EPSF
FALSE
// nodeShadows
TRUE
// showNodeBorders
TRUE
// shadowOffsetWorldX
4
// shadowOffsetWorldY
-4
// units
INCHES
// nodeColor
1.000000 1.000000 0.000000
// nodeBorderColor
0.000000 0.000000 0.000000
// nodeShadowColor
0.500000 0.500000 0.000000
// nodeFontColor
0.000000 0.000000 0.000000
// nodeGray
0.750000
// nodeBorderGray
0.000000
// nodeShadowGray
0.550000
// nodeFontGray
0.000000
// edgeColor
0.000000 0.000000 1.000000
// edgeGray
0.000000
// edgeLineWidth
0.500000
// desiredPageWidth
8.500000
// desiredPageHeight
11.000000
// pageBorder
0.500000
// borderLineWidth
1.000000
// nodeFont
Helvetica
// nodeFontSizeWorld
14.000000
// pageLabelFont
Helvetica-Bold
// pageLabelFontSize
10.000000
// pageNumberFont
Helvetica
// pageNumberFontSize
8.000000
// showNodeText
FALSE
// sizeNodesToText
FALSE
// arrowHeadLengthWorld
10
// arrowBodyThicknessPercent
30
// arrowHeadThicknessPercent
60
// arrowHeadLengthPercent
80
// worldUnitsPerInch
72
// deviceUnitsPerInch
72
// useScaleFactor
FALSE
// scaleFactor
100
// numberOfPageColumns
1
// numberOfPageRows
1
// rotate
FALSE
// fillPages
TRUE
// color
FALSE
// showGraphBorder
TRUE
// showCropMarks
TRUE
// showPageLabels
TRUE
// showPageNumbers
TRUE
// showLabelRect
FALSE
// showLabelText
INSIDE
// sizeLabelsToText
FALSE
// labelShadows
FALSE
// showLabelBorders
FALSE
// labelGray
0.950000
// labelBorderGray
0.200000
// labelShadowGray
0.750000
// labelFontGray
0.000000
// labelColor
1.000000 1.000000 0.600000
// labelBorderColor
0.400000 0.400000 0.400000
// labelShadowColor
0.700000 0.700000 0.200000
// labelFontColor
0.200000 0.200000 0.200000
// labelFontSizeWorld
10.000000
// labelFont
Helvetica
// Digraph
// lastLayoutStyle
HIERARCHICAL
// layoutStyle
HIERARCHICAL
// Begin Editor Fonts
-16;0;0;0;0;0;0;0;1;0;0;0;0;Courier New
// End Editor Fonts

// Editor Digraph
Version 3.0
$basename
// Digraph Background Color
16777215
// Topology
// coordinates
TRUE

END_OF_HEADER

}


sub end_GET_flowchart() {
  print "\n";
}


# Returns: ($basename, \@nodes, \@edges)

sub read_GET_graph($) {
  my ($file) = @_;
  local $_ = "";
  local *IN;
  my ($keyword, $value, %pars);
  my $header = "";
  my @nodes = ();
  my @edges = ();
  if ($file eq "") {
    $GET::filename = "<STDIN>";
    open(IN, "<&STDIN");
  } else {
    $GET::filename = $file;
    open(IN, $file) or die "File $file not found: $!";
  }
  while (<IN>) {
    last if (m!^// Editor Digraph$!);
  }
  die "$GET::filename does not appear to be a GET file!\n"
    if (eof(IN));
  $_ = <IN>; # Skip version number
  my $basename = <IN>;
  chomp($basename);
  skipto("// Nodes");
  $_ = <IN>; # Blank line
  $_ = <IN>; # Start of first node (if any)
  for (;;) {
    last unless (m!^// Node$!);
    %pars = ();
    while (<IN>) {
      last if /\S/;
    }
    check("// name");
    chomp($pars{title} = <IN>);
    $pars{title} =~ s/^"(.*)"$/$1/;
    $_ = <IN>;
    check("// height");
    $_ = <IN>; $_ = <IN>;
    check("// width");
    $_ = <IN>; $_ = <IN>;
    check("// Editor Node");
    $_ = <IN>; # Skip version number;
    chomp($pars{label} = <IN>);
    $pars{label} =~ s/\~/\n/g;
    chomp($_ = <IN>);
    graph_error("Unexpected node shape: $_") unless ($VCG_node_shape{$_});
    $pars{shape} = $VCG_node_shape{$_};
    $pars{label} = undo_shift($pars{shape}, $pars{label});
    chomp($_ = <IN>);
    graph_error(qq["0x0" expected]) unless ($_ eq "0x0");
    chomp($_ = <IN>);
    graph_error("Unexpected node colour: $_") unless defined($VCG_colour{$_});
    $pars{colour} = $VCG_colour{$_};
    chomp($_ = <IN>);
    graph_error(qq["1" expected]) unless ($_ eq "1");
    # Store this node:
    push(@nodes, [$pars{shape} || "box", $pars{title}, $pars{label},
		  $pars{info1} || "",  $pars{info2} || "", $pars{info3} || "",
		  "", $pars{colour} || ""]);
    $_ = <IN>;
    last if (m!^// Edges$!);
    graph_error(qq[blank line expected]) unless (/^\s*$/);
    $_ = <IN>;
  }
  check("// Edges");
  $_ = <IN>; # Start of first edge (if any)  
  for (;;) {
    check("// Edge");
    %pars = ();
    while (<IN>) {
      last if /\S/;
    }
    check("// fromNodeName");
    chomp($pars{sourcename} = <IN>);
    $pars{sourcename} =~ s/^"(.*)"$/$1/;
    $_ = <IN>;
    check("// toNodeName");
    chomp($pars{targetname} = <IN>);
    $pars{targetname} =~ s/^"(.*)"$/$1/;
    $_ = <IN>;
    if (m!// Labels$!) {
      skipto("// name");
      chomp($pars{label} = <IN>);
    }
    push(@edges, ["edge", $pars{sourcename}, $pars{targetname},
		  $pars{label}     || "", $pars{class}    || "",
		  $pars{linestyle} || "", $pars{priority} || "",
		  $pars{thickness} || "", $pars{color} || ""]);
    # Skip to start of next edge or end of file:
    next if (m!// Edge$!);
    while (<IN>) {
      last if (m!^// Edge$!);
    }
    last if (eof(IN));
  }
  return($basename, \@nodes, \@edges);
}


# Undo the "shift" spaces added to non-box node labels:

sub undo_shift($$) {
  my($shape, $label) = @_;
  return($label) if ($shape eq "box");
  my $shift = 0;
  my @lines = split(/\n/, $label);
  my $chars = max(map { length($_) } @lines);
  if ($shape eq "rhomb") {
    $shift = $chars - int(2*$chars / 3);
  } elsif ($shape eq "ellipse") {
    $shift = 1;
  } elsif ($shape eq "triangle") {
    $shift = $chars - int(2*$chars / 3);
  } elsif ($shape eq "twobar1") {
    $shift = 3;
  } elsif ($shape eq "twobar2") {
    $shift = 3;
  } else {
    graph_error("Unknown shape: $shape");
  }
  $shift = " " x $shift;
  foreach (@lines) {
    s/^$shift//;
  }
  return(join("\n",  @lines));
}

  
sub skipto($) {
  my ($pat) = @_;
  while (<IN>) {
    last if (/^$pat$/);
  }
  graph_error(qq["$pat" expected]) if (eof(IN));
}


sub check($) {
  my ($str) = @_;
  $_ =~ s/\s+$//;
  graph_error(qq["$str" expected]) unless($_ eq $str);
}
  


sub graph_error($) {
  my ($message) = @_;
  die "GET syntax error: $message in line $. of file: $GET::filename\n<$_>\n";
}


1;

