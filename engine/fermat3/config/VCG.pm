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
# Perl module to read, process and generate VCG files
#

package VCG;

require 5.000;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(read_graph print_flowchart print_callgraph make_label
	     max_incoming delete_singletons delete_simple_branches
	     dataflow_edges find_reachable);

use strict;

# The nodes list is a list of array refs:
# [shape, title, label, WSL, comments, code, edge_label, colour]
#
# The edges list is also a list of array refs:
# [type, source, target, label, pri, class, style, thick, colour]
#

$VCG::schar = '[A-Za-z0-9\@\$\#\&\_\x80-\xff]';

sub init_callgraph($$) {
  my ($layout, $orientation) = @_;
  if ($layout eq "normal") {
    $layout = "";
  } else {
    $layout = "layoutalgorithm: $layout";
  }
  print <<END;

graph: { title: "Graph"


  width: 1600
  height: 1200
  x: 0 
  y: 0

  $layout

  color: white
  edge.thickness: 4

  title: "Graph"
  xmax: 1600
  ymax: 1200
  xbase: 5
  ybase: 5
  xspace: 20
  xlspace: 10
  yspace: 20
  xraster: 1
  xlraster: 1
  yraster: 1
  shrink:  1
  stretch: 1
  layout_downfactor: 1
  layout_upfactor: 1
  layout_nearfactor: 1
  layout_splinefactor: 70
  spreadlevel: 1
  treefactor: 0.500000
  bmax: 32767
  cmin: 0
  cmax: 32767
  pmin: 0
  pmax: 32767
  rmin: 0
  rmax: 32767
  smax: 32767
  node_alignment: center
  orientation: $orientation
  late_edge_labels: no
  display_edge_labels: yes
  dirty_edge_labels: yes
  finetuning: yes
  nearedges: yes
  splines: no
  ignoresingles: no
  straight_phase: yes
  priority_phase: yes
  manhatten_edges: no
  smanhatten_edges: no
  port_sharing: no
  crossingphase2: yes
  //crossingoptimization: yes
  crossingoptimization: no
  crossingweight: bary
  arrow_mode: free
  colorentry 7 :  85 85 85
  colorentry 8 :  0 0 128
  colorentry 9 :  128 0 0
  colorentry 10 :  0 128 0
  colorentry 11 :  128 128 0
  colorentry 12 :  128 0 128
  colorentry 13 :  0 128 128
  colorentry 14 :  255 215 0
  colorentry 15 :  170 170 170
  colorentry 16 :  128 128 255
  colorentry 17 :  255 128 128
  colorentry 18 :  128 255 128
  colorentry 19 :  255 255 128
  colorentry 20 :  255 128 255
  colorentry 21 :  128 255 255
  colorentry 22 :  238 130 238
  colorentry 23 :  64 224 208
  colorentry 24 :  127 255 212
  colorentry 25 :  240 230 140
  colorentry 26 :  160 32 240
  colorentry 27 :  154 205 50
  colorentry 28 :  255 192 203
  colorentry 29 :  255 165 0
  colorentry 30 :  218 112 214

  classname 1 : "normal"
  classname 2 : "from dispatch"
  classname 3 : "to dispatch"


END
}



sub end_callgraph() {
  print "\n}\n";
}



sub init_flowchart() {
  print <<'END';
graph: { title: "Flowchart"


  width: 1600
  height: 1200
  x: 0 
  y: 0

  manhatten_edges: yes
  smanhatten_edges: no
  layoutalgorithm: minbackward
  // layoutalgorithm: dfs
  port_sharing: yes
  edge.thickness: 4
  foldnode.textcolor: red

  color: white
  xmax: 1600
  ymax: 1200
  xbase: 5
  ybase: 5
  xspace: 20
  xlspace: 12
  yspace: 50
  xraster: 1
  xlraster: 1
  yraster: 1
  shrink:  1
  stretch: 1
  layout_downfactor: 100
  layout_upfactor: 1
  layout_nearfactor: 0
  layout_splinefactor: 70
  spreadlevel: 1
  treefactor: 0.500000
  bmax: 50
  cmin: 0
  cmax: 10
  pmin: 0
  pmax: 20
  rmin: 0
  rmax: 30
  smax: 50
  node_alignment: center
  orientation: top_to_bottom
  late_edge_labels: no
  display_edge_labels: yes
  dirty_edge_labels: yes
  finetuning: yes
  nearedges: yes
  splines: no
  ignoresingles: no
  straight_phase: yes
  priority_phase: yes
  crossingphase2: yes
  //crossingoptimization: yes
  crossingoptimization: no
  crossingweight: barymedian
  arrow_mode: free
  colorentry 7 :  85 85 85
  colorentry 8 :  0 0 128
  colorentry 9 :  128 0 0
  colorentry 10 :  0 128 0
  colorentry 11 :  128 128 0
  colorentry 12 :  128 0 128
  colorentry 13 :  0 128 128
  colorentry 14 :  255 215 0
  colorentry 15 :  170 170 170
  colorentry 16 :  128 128 255
  colorentry 17 :  255 128 128
  colorentry 18 :  128 255 128
  colorentry 19 :  255 255 128
  colorentry 20 :  255 128 255
  colorentry 21 :  128 255 255
  colorentry 22 :  238 130 238
  colorentry 23 :  64 224 208
  colorentry 24 :  127 255 212
  colorentry 25 :  240 230 140
  colorentry 26 :  160 32 240
  colorentry 27 :  154 205 50
  colorentry 28 :  255 192 203
  colorentry 29 :  255 165 0
  colorentry 30 :  218 112 214

  colorentry 32 :  192 192 192

  classname 1 : "normal"
  classname 2 : "from dispatch"
  classname 3 : "to dispatch"

  infoname 1 : "WSL"
  infoname 2 : "comments"
  infoname 3 : "assembler"

END
}


sub end_flowchart() {
  print "\n}\n";
}


# Process a graph to add "connectors" for nodes with a lot
# of incoming edges and delete singleton nodes:

sub max_incoming_old($$) {
  my ($nodes, $edges) = @_;
  # Process the @nodes and @edges lists:
  my %edge_count = ();
  my $extra = 0;	# counter for extra nodes
  my ($node, $edge, $l);
  foreach $edge (@{$edges}) {
    $edge_count{$$edge[2]}++;	# 2=target node
  }
  if ($::S_Max_Incoming_Edges) {
    foreach $node (@{$nodes}) {
      next unless($edge_count{$$node[1]});
      if ($edge_count{$$node[1]} > $::S_Max_Incoming_Edges) {
	# Create new triangle nodes and modify the incoming edges:
	$extra++;
	$$node[6] = get_label($$node[5]) unless ($$node[6]);
	$l = $$node[6] || "$extra";
	push(@{$nodes}, ["triangle", "tri$extra", $l, "", "", "", $l, ""]);
	push(@{$edges}, ["nearedge", "tri$extra", $$node[1], "", "", "", "", "", ""]);
	# Redirect the edges to new triangle nodes:
	foreach $edge (@{$edges}) {
	  next if ($$edge[1] =~ /^tri\d+$/);
	  if ($$edge[2] eq $$node[1]) {
	    $extra++;
	    push(@{$nodes}, ["triangle", "tri$extra", $l, "", "", "", "", ""]);
	    $$edge[2] = "tri$extra";
	    # Change to a neearedge unless there is a label:
	    $$edge[0] = "nearedge" unless ($$edge[3]);
	  }
	}
      }
    }
  }
}

sub max_incoming($$) {
  my ($nodes, $edges) = @_;
  # Process the @nodes and @edges lists:
  my %edge_count = ();
  my $extra = 0;	# counter for extra nodes
  my $name = "a";	# name for the connector (use magic increment)
  my ($node, $edge, $l);
  foreach $edge (@{$edges}) {
    $edge_count{$$edge[2]}++;	# 2=target node
  }
  if ($::S_Max_Incoming_Edges) {
    foreach $node (@{$nodes}) {
      next unless($edge_count{$$node[1]});
      if ($edge_count{$$node[1]} > $::S_Max_Incoming_Edges) {
	# Create new ellipse nodes and modify the incoming edges:
	$extra++;
	push(@{$nodes}, ["ellipse", "tri$extra", $name, "", "", "", "", ""]);
	push(@{$edges}, ["nearedge", "tri$extra", $$node[1], "", "", "", "", "", ""]);
	# Redirect the edges to new ellipse nodes, apart from fallthrough edges:
	foreach $edge (@{$edges}) {
	  next if ($$edge[7] eq "5");
	  next if ($$edge[1] =~ /^tri\d+$/);
	  if ($$edge[2] eq $$node[1]) {
	    $extra++;
	    push(@{$nodes}, ["ellipse", "tri$extra", $name, "", "", "", "", ""]);
	    $$edge[2] = "tri$extra";
	    # Change to a neearedge unless there is a label:
	    $$edge[0] = "nearedge" unless ($$edge[3]);
	  }
	}
	$name++;
      }
    }
  }
}


sub delete_singletons($$) {
  my ($nodes, $edges) = @_;
  my ($node, $edge);
  # Delete singleton nodes:
  my %ends = ();
  foreach $edge (@{$edges}) {
    $ends{$$edge[1]}++;
    $ends{$$edge[2]}++;
  }
  foreach $node (@{$nodes}) {
    next if ($$node[0] =~ /^(begin|end)$/);
    $node = "" unless ($ends{$$node[1]});
  }
}

# If a node only contains a single unconditional branch,
# or a single ENDIF mnemonic
# or contains only a mnemonic and is an unconditional branch,
# then delete it. All edges to this node are redirected:

sub delete_simple_branches($$$@) {
  my ($nodes, $edges, $branch, @if_end) = @_;
  my ($node, $edge, $title, $target);
  my %if_end;
  foreach (@if_end) { s/h/#/g; $if_end{$_}++ };
  foreach $node (@$nodes) {
    next unless($node);
    next unless($$node[1] =~ /^\d+$/);
    next if ($$node[5] =~ /\n/);
    if (($$node[5] eq "")
	 || ($$node[5] =~ /^$::schar*\s+B\s+$::schar+(\s|$)/)
	 || (($$node[5] =~ /^$::schar*\s+(\S+)/) && $if_end{$1})
	 || (($$node[5] =~ /^\s*[A-Z]\S+$/) && defined($$branch[$$node[1]])
	       && ($$branch[$$node[1]] == 2))
	) {
      print STDERR "Deleting $$node[1]: <$$node[2]>\n";
      $title = $$node[1];
      $node = "";
      foreach $edge (@$edges) {
	next unless($edge);
	if ($$edge[1] eq $title) {
	  $target = $$edge[2];
	  $edge = "";
	  last;
	}
      }
      foreach $edge (@$edges) {
	next unless($edge);
	$$edge[2] = $target if ($$edge[2] eq $title);

      }
    }
  }
}



# Add dataflow edges to the flowchart
# from <FermaT><...><...SMLddddd...> nodes to SMLddddd sequence numbers:

sub dataflow_edges($$) {
  my ($nodes, $edges) = @_;
  my ($node, $edge, $title, $code, $seq, $var, $stuff, $target, $l, $col);
  my $data_colour = "red";
  # First map SML sequence numbers to the corresponding node:
  my %seq_node = ();
  foreach $node (@{$nodes}) {
    next unless ($node);
    $title = $$node[1];
    $code = $$node[4] || $$node[5];
    $code =~ s/(SML\d{5})\s*$/$seq_node{$1} = $title/gem;
  }
  # Now add dataflow edges:
  my %edge_count = ();
  my %done = ();
  # Record the set of labels of dataflow edges into each node:
  my %node_edge_label = ();
  foreach $node (@{$nodes}) {
    next unless ($node);
    $title = $$node[1];
    $code = $$node[4] || $$node[5];
    while ($code =~ s/(<FermaT><)([^<>]*)(><[^<>]*)(SML\d{5})/$1$2$3/) {
      $var = $2;
      $stuff = $3;
      $seq = $4;
      if ($stuff =~ /($VCG::schar+)\@$/) {
	$var = $1;
      }
      $target = $seq_node{$seq};
      next if ($title eq $target);
      next if ($done{$title,$target,$var}++);
      $edge_count{$target}++;
      $node_edge_label{$target}{$var}++;
      #print STDERR "dataflow $title --> $target\n";
      push(@{$edges}, ["backedge", $title, $target, $var, 0, "", "", 2, $data_colour]);
    }
  }

  # Fix dataflow edges where a node has too many incoming edges:
  my $extra = 0;
  if ($::S_Max_Incoming_Edges) {
    foreach $node (@{$nodes}) {
      next unless ($node);
      $title = $$node[1];
      next unless($edge_count{$title});
      if ($edge_count{$title} > $::S_Max_Incoming_Edges) {
	# Create new triangle nodes (one for each variable involved in dataflow):
	$$node[6] = get_label($$node[5]) unless ($$node[6]);
	foreach $var (keys %{$node_edge_label{$title}}) {
	  $extra++;
	  # $l is the label for all the triangles for this node:
	  $l = $$node[6] || $extra;
	  $node_edge_label{$title}{$var} = $l;
	  push(@{$nodes}, ["triangle", "dat$extra", $l, "", "", "", "", ""]);
	  push(@{$edges}, ["backedge", "dat$extra", $title, $var, "", "", "",
			   2, $data_colour]);
	}
	# Redirect the dataflow edges entering this node to new triangle nodes:
	foreach $edge (@{$edges}) {
	  next if ($$edge[1] =~ /^dat\d+$/);
	  next unless ($$edge[8] eq $data_colour);
	  if ($$edge[2] eq $title) {
	    $extra++;
	    $l = $node_edge_label{$title}{$$edge[3]};
	    push(@{$nodes}, ["triangle", "dat$extra", $l, "", "", "", "", ""]);
	    # Redirect the target of the edge to the new triangle node:
	    $$edge[2] = "dat$extra";
	  }
	}
      }
    }
  }
}


# Search for a label (or #SUBR or #LOCA parameter) in the given assembler code:

sub get_label($) {
  my ($code) = @_;
  my $line;
  foreach $line (split(/\n/, $code)) {
    next if ($line =~ /^\*/);
    return ($&) if ($line =~ /^$VCG::schar+/);
    return($2) if ($line =~ /^\s+(#SUBR|#LOCA)\s+($VCG::schar+)/);
  }
  return("");
}


sub get_seq($) {
  my ($code) = @_;
  my $line;
  foreach $line (split(/\n/, $code)) {
    return($2) if ($line =~ /(^|\s)(\d{8}|SML\d{5})\s*$/);
  }
  return("");
}


sub print_flowchart($$) {
  my ($nodes, $edges) = @_;
  my ($node, $edge);
  init_flowchart();
  foreach $node (@{$nodes}) {
    vcg_node(@{$node}) if ($node);
  }
  foreach $edge (@{$edges}) {
    vcg_edge(@{$edge}) if ($edge);
  }
  end_flowchart();
}


sub print_callgraph($$$$) {
  my ($layout, $orientation, $nodes, $edges) = @_;
  my ($node, $edge);
  init_callgraph($layout, $orientation);
  foreach $node (@{$nodes}) {
    vcg_node(@{$node}) if ($node);
  }
  foreach $edge (@{$edges}) {
    vcg_edge(@{$edge});
  }
  end_callgraph();
}



# shape is ellipse, box or rhomb

sub vcg_node($$$$$$$$) {
  my ($shape, $title, $label, $wsl, $comments, $code, $l, $col) = @_;
  if (($shape eq "twobar1") || ($shape eq "twobar2")) {
    # These shapes are not available for a VCG:
    $shape = "box";
    $col = 32;
  }
  print "\n";
  if ($shape eq "begin") {
    print qq[graph: { title: "$title" folding: $label horizontal_order: $wsl\n];
    return();
  } elsif ($shape eq "end") {
    print "}\n\n";
    return();
  }
  # Protect quotes and backslashes:
  my $str;
  foreach $str (($title, $label, $wsl, $code, $comments)) {
    $str =~ s/\\/\\\\/g;
    $str =~ s/\"/\\\"/g;
  }
  my $info = "";
  $info = qq[info1: "$wsl" ] if ($wsl ne "");
  $info .= qq[info2: "$comments" ] if ($comments ne "");
  $info .= qq[info3: "$code" ] if ($code ne "");
  $info .= qq[color: $col ] if ($col ne "");
  print qq[node: { shape: $shape title: "$title" label: "$label" $info }\n];
END
}


sub vcg_edge($$$$$$$$$) {
  my ($type, $source, $target, $label, $pri,
      $class, $style, $thick, $colour) = @_;
  $thick = 1 if (($pri ne "") && ($thick eq ""));
  $label = qq[label: "$label"] if ($label ne "") ;
  $pri = "priority: $pri" if ($pri ne "");
  $thick = "thickness: $thick" if ($thick ne "");
  $class = "class: $class" if ($class ne "");
  $style = "linestyle: $style" if ($style ne "");
  $colour = "color: $colour" if ($colour ne "");
  my $extra = "$class $style $pri $thick $colour";
  $extra =~ s/\s+/ /g;
  print qq[$type: { sourcename: "$source" targetname: "$target" $label $extra }\n];
}



sub make_label($$$) {
  my ($shape, $code, $comments) = @_;
  my ($linelen, $min, $str, $space, @trim, $elided, $pad);
  if (($shape eq "box") || ($shape eq "twobar1") || ($shape eq "twobar2")) {
    $linelen = $::S_Max_Box_Chars;
  } else {
    $linelen = $::S_Max_Rhomb_Chars;
  }
  # Loop with a smaller $linelen if the result is still too big:
  for (;;) {
    if ($code =~ /\S/) {
      @trim = split(/\n/, $code);
    } else {    
      # Use the comments for the label:
      @trim = split(/\n/, $comments);
    }
    return("") unless (@trim);
    if (@trim > $::S_Max_Box_Lines1 + $::S_Max_Box_Lines2 + 1) {
      # Elide some lines from the list:
      $elided = @trim - ($::S_Max_Box_Lines1 + $::S_Max_Box_Lines2);
      @trim = (@trim[0 .. $::S_Max_Box_Lines1 - 1], "",
	       @trim[$#trim - $::S_Max_Box_Lines2 + 1 .. $#trim]);
    }
    compress_cols(\@trim);
    $trim[$::S_Max_Box_Lines1] = "...($elided lines)" if ($elided);

    # Finally, trim the lines that are still too long
    # (if this would leave a blank comment, then delete the leading
    # spaces in the comment first):
    my $l = $linelen - 2;
    foreach $str (@trim) {
      $str =~ s/^\* {$l,}/\* /;
      $str = substr($str, 0, $linelen - 1) . "|" if (length($str) > $linelen);
    }
    $str = join("\n", @trim);
    if ($shape eq "box") {
      $pad = 0;
    } elsif (($shape eq "twobar1") || ($shape eq "twobar2")) {
      $pad = 3 * @trim;
    } else {
      $pad = int(@trim/2 + 0.5) * @trim;
    }

    last if (length($str) + $pad < 256);
    # Work around GET's 255 character limit for labels:
    $linelen = int((255 - $pad)/($::S_Max_Box_Lines1+$::S_Max_Box_Lines2+1)) - 1;
  }
  return($str);
}



sub compress_cols($) {
  my ($trim) = @_;
  # Compress columns of space.
  # Pad each line to $len and OR the characters together
  # Any sequences of spaces in the result represent columns
  # of space in the original.
  # NOTE: Comment lines are not included:
  # we simply compress all spaces on comment lines.
  my ($len, $p, $n);
  local $_;
  $len = max(map { length($_) } @$trim);
  my $space = " " x $len;
  foreach (@$trim) {
    if (/^\*/) {
      s/ +/ /g;
    } else {
      $space |= $_ . " " x ($len - length($_));
    }
  }
  while ($space =~ s/ {2,}/ /) {
    $p = length($`);
    $n = length($&);
    foreach (@$trim) {
      next if (/^\*/);
      substr($_, $p, $n - 1) = "" if (length($_) > $p);
    }
  }
}



# Read a VCG graph and return references to the nodes and edges lists
# (this ignores the main graph options, subgraph layouts and any
# other unsupported options)

sub read_graph($) {
  my ($file) = @_;
  local $_ = "";
  local *IN;
  my ($keyword, $value, $type, %pars);
  my @nodes = ();
  my @edges = ();
  my $end_graph = 0;	# = 1 if the next "}" will end a graph
  if ($file eq "") {
    $VCG::filename = "<STDIN>";
    open(IN, "<&STDIN");
  } else {
    $VCG::filename = $file;
    open(IN, $file) or die "File $file not found: $!";
  }
  # Skip to a keyword:
  $keyword = get_keyword();
  for (;;) {
    last unless $keyword;
    if ($keyword eq "}") {
      # End a graph (unless we are in a node or an edge)
      push(@nodes, ["end", "", "", "", "", "", "", "", ""]) if ($end_graph);
      $end_graph = 1;
      $keyword = get_keyword();
      next;
    } elsif ($keyword =~ /^(graph|node|edge|nearedge|bentnearedge|backedge)$/) {
      $type = $keyword;
      # Read the { and all parameters up to closing }:
      %pars = ();
      for (;;) {
	$keyword = get_keyword();
	graph_error(qq["}" expected]) unless defined($keyword);
	last if ($keyword eq "}");
	last if ($keyword =~ /^(graph|node|edge|nearedge|bentnearedge|backedge)$/);
	$value = get_value();
	$pars{$keyword} = $value;
      }
      if ($type eq "graph") {
	$end_graph = 1;
	push(@nodes, ["begin", $pars{title}, $pars{folding} || 0,
		      $pars{horizontal_order} || 0]);
      } elsif ($type eq "node") {
	$end_graph = 0;
	push(@nodes, [$pars{shape} || "box", $pars{title}, $pars{label},
		      $pars{info1} || "",  $pars{info2} || "", $pars{info3} || "",
		      "", $pars{color} || ""]);
      } else {
	$end_graph = 0;
	push(@edges, [$type, $pars{sourcename}, $pars{targetname},
		      $pars{label}     || "", $pars{class}    || "",
		      $pars{linestyle} || "", $pars{priority} || "",
		      $pars{thickness} || "", $pars{color} || ""]);
      }
    } else {
      # Skip the value and continue
      $value = get_value();
    }
  }
  # Trim the initial begin...end pair:
  if ($nodes[0][0] eq "begin") {
    shift(@nodes);
    pop(@nodes);
  }
  return(\@nodes, \@edges);
}


# Read up to the next "}" or "keyword:"

sub get_keyword() {
  my $keyword = "";
  for (;;) {
    next if s/^\s+//;
    next if s/^\/\/.*$//;
    next if s/^\{//;
    if (s/^\}//) {
      $keyword = $&;
      last;
    }
    if (s/^([a-z][a-z0-9_\.]*(\s+\d+)?)\s*:\s*//) {
      $keyword = $1;
      last;
    }
    # There shouldn't be anything here:
    graph_error("keyword not found") if (/\S/);
    $_ = <IN>;
    last unless defined($_);
  }
  return($keyword);
}


# Get a value, either a simple word or three numbers or a string:

sub get_value() {
  my $value = "";
  return($&) if s/^\d+\s+\d+\s+\d+//;
  return($&) if s/^[a-zA-Z0-9_\.]+//;
  graph_error("malformed value") unless s/^"//;
  for(;;) {
    if (s/^([^\\"]*\\\\)+//) {
      $value .= $&;
      next;
    } elsif (s/^([^\\"]*\\")+//) {
      $value .= $&;
      next;
    } elsif (s/^[^\\"]+//) {
      $value .= $&;
      next;
    } elsif (s/^([^\\"]*)"//) {
      $value .= $1;
      return($value);
    } elsif ($_ eq "") {
      $_ = <IN>;
      graph_error("File ended inside a string") unless defined($_);
      next;
    } else {
      graph_error("End of string not found");
      return($value);
    }
  }
}


sub graph_error($) {
  my ($msg) = @_;
  die "VCG syntax error: $msg in line $. of file: $VCG::filename\n<$_>\n";
}

# Find the nodes reachable from the given set of node labels
# and delete everything else:

sub find_reachable($$$) {
  my ($start, $nodes, $edges) = @_;
  my ($node, $edge);
  my ($i, $n, $m);
  my %wanted = ();
  my %linked = ();
  my %title_index = ();
  my @newnodes = ();
  my @newedges = ();
  my @agenda = ();

  # Initialise the agenda:
  for ($i = 0; $i <= $#{$nodes}; $i++) {
    $node = $$nodes[$i];
    next unless ($node);
    $title_index{$$node[1]} = $i;
    if ($$start{$$node[2]}) {
      push(@agenda, $i) unless ($wanted{$i}++);
    }
  }

  # Compute the set of nodes linked to each node:
  foreach $edge (@{$edges}) {
    $linked{$title_index{$$edge[1]}}{$title_index{$$edge[2]}}++;
    $linked{$title_index{$$edge[2]}}{$title_index{$$edge[1]}}++;
  }

  while (@agenda) {
    $n = shift(@agenda);
    # Add any new nodes which are linked to $n to the agenda:
    foreach $m (keys %{$linked{$n}}) {
      push(@agenda, $m) unless ($wanted{$m}++);
    }
  }

  # Build new node and edge lists:
  for ($i = 0; $i <= $#{$nodes}; $i++) {
    push(@newnodes, $$nodes[$i]) if ($wanted{$i});
  }
  
  foreach $edge (@{$edges}) {
    push(@newedges, $edge) if ($wanted{$title_index{$$edge[1]}}
				 || $wanted{$title_index{$$edge[2]}});
  }

  @{$nodes} = @newnodes;
  @{$edges} = @newedges;

}


sub min(@) {
  my $result = $_[0];
  my $val;
  foreach $val (@_) {
    $result = $val if ($val < $result);
  }
  return($result);
}


sub max(@) {
  my $result = $_[0];
  my $val;
  foreach $val (@_) {
    $result = $val if ($val > $result);
  }
  return($result);
}

1;

