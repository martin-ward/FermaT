#!/usr/bin/perl

# ***********************************************
#
#   File: generate_transformation_table.pl
#
#   Author: Matthias Ladkau
#
#   Description:
#
#     This is a script for generating the XML transformation
#     list from the FermaT engine source code
#
# ***********************************************

use strict;
use warnings;

sub scm_name($);

undef $/; # enable slurp mode

print "<TransformationTable>\n";

my @list = <../fermat3/src/trans/*_d.wsl>;
foreach my $file (@list) {

  my $id = "";
  my $name = "";
  my $desc = "";
  my $keyw = "";

  open(FILE,"<", $file);
  $_ = <FILE>;
  close(FILE);

  s/\n/ /g;
  s/[\'\`\<\>]//g;

  $id   = scm_name($1) if /\b(TR_\w+)\s*:=\s*\@New_TR_Number/;
  $name = $1 if /\bTRs_Name\[.*?\]\s*:=\s*\"(.*?)\"/;
  $desc = $1 if /\bTRs_Help\[.*?\]\s*:=\s*\"(.*?)\"/;
  $keyw = $1 if /\bTRs_Keywords\[.*?\]\s*:=\s*(.*?)\s*;/;
  $keyw =~ s/[\",]//g;
  $keyw =~ s/(L_to_R|R_to_L)\s*//g;
  next if $keyw =~ /\bHidden\b/;

  # Hunt for the nodes this transformation focuses on

  my %nodes = ();
  $file =~ s/_d.wsl/.wsl/g;
  open(FILE,"<", $file);
  $_ = <FILE>;
  close(FILE);
  s/\n/ /g;
  s/^.*_Test\s*\(\)\s*==(.*?)MW_PROC.*$/$1/ or die $_;
  $nodes{$1}++ while s/\b(T_[A-Za-z_]+)\b/ /;

  my $nod = join("#", sort keys %nodes);
  print qq[  <Transformation id="$id" name="$name" description="$desc" keywords="$keyw" nodes="$nod"/>\n];

}

print "</TransformationTable>\n";

# Convert a WSL name to a Scheme name:
sub scm_name($) {
  my ($name) = @_;
  $name =~ s#([A-Z])#/$1#g;
  return("/$name");
}

