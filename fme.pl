#!/usr/bin/perl
# ***********************************************
#
#   File: fme.pl
#
#   Author: Martin Ward
#
#   Description:
#
#     This is a start script for the
#     FermaT Maintenance Environment
#
# ***********************************************

my $dir = ".";
$dir = $1 if $0 =~ m|(^.*)/[^/]*|;
chdir $dir or die "Cannot change to $dir directory: $!\n";

my $java = ($ENV{JAVA} || "java");
my $path = "./lib/fme.jar:./lib/ave.jar:./lib/toniclf.jar:./lib/substance.jar:"
  . "./lib/dynamic_typed_wsl.jar:./lib/simple_typed_wsl.jar:"
  . "./lib/weak_unsafe_typed_wsl.jar:./lib/strong_unsafe_typed_wsl.jar:"
  . "./lib/xercesImpl.jar:./lib/xmlParserAPIs.jar:./lib/stringsearch.jar:"
  . "./lib/sve.jar:./lib/zvtm.jar";

exec $java, "-classpath", $path, "fme.FMEMain";
