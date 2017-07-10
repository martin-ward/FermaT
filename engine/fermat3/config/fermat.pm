#
# Common routines for FermaT scripts.
#
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

use Cwd;
use Config;

$| = 1; # Unbuffered output (for tee)

sub read_option_file($);
sub mysystem($@);

$SIG{INT} = \&cleanup_goners;
$SIG{HUP} = \&cleanup_goners if defined($SIG{HUP});
$SIG{QUIT} = \&cleanup_goners;
$SIG{TERM} = \&cleanup_goners;

# Process arguments with quotes and get the FermaT argument, if present:
my @argv = ();
$FermaT = "";
$quiet = 0;
while (@ARGV) {
  $arg = shift(@ARGV);
  while (($arg =~ /^"[^"]*$/) && @ARGV) {
    $arg .= " " . shift(@ARGV);
  }
  $arg =~ s/"//g; #"
  $arg =~ s/\\+$//g;
  push(@argv, $arg);
}
while (@argv) {
  $arg = shift(@argv);
  if ($arg eq "-FermaT") {
    die "Malformed -FermaT argument\n" unless @argv;
    $FermaT = shift(@argv);
  } elsif ($arg eq "-bigloo") {
    $ENV{SCM_IMPL} = "bigloo";
  } elsif ($arg eq "-parallel") {
    $ENV{FermaT_parallel} = 1;
    $ENV{SHELL} = "/bin/bash";
  } elsif ($arg eq "-newmem") {
    $ENV{Mem_Version} = 1;
  } elsif ($arg eq "-cdir") {
    die "Malformed -cdir argument\n" unless @argv;
    my $dir = shift(@argv);
    chdir($dir) or die "Can't chdir $dir: $!\n";
  } elsif ($arg =~ /^-(q|s|-quiet|-silent)$/) {
    $quiet = 1;
  } else {
    push(@ARGV, $arg);
  }
}
if (!$FermaT) {
  $FermaT = $ENV{'FermaT'} || "/usr/local/fermat2";
  $FermaT =~ s/"//g; #"
}
die "FermaT directory `$FermaT' does not exist!\n" unless -d $FermaT;

$dos = 0;
$exe = 0;
if ($^O eq "solaris") {
  $arch = "SunOS";
} elsif ($^O eq "aix") {
  $arch = "AIX";
} elsif ($^O eq "linux") {
  $arch = "Linux";
  if (chomp($machine = `arch`)) {
    if ($machine =~ /^i[3456]86$/) {
      # We are on Intel Linux.
      # Use our version of Bit::Vector if it is not installed:
      push(@INC, "$FermaT/config/i386-linux");
    }
    if ($machine eq "armv6l") {
      $arch = "arm";
    }
  }
} elsif ($^O eq "darwin") {
  $arch = "Darwin";
} elsif ($^O eq "hpux") {
  $arch = "HP-UX";
} elsif ($^O eq "cygwin") {
  $arch = "Cygwin";
  $exe = 1;
} elsif ($^O eq "MSWin32") {
  $arch = "MinGW";
  $dos = 1;
  $exe = 1;
} else {
  die "Unsupported operating system: $^O\n";
}

if (!$dos) {
  # We are on a sensible operating system
  $ds = "/";
  $tmpdir = "/tmp/";
  $exec = qq["$FermaT/$arch/scmfmt" -p0];
  $gambit_exec = "gsi";
  $gambit_file = "$FermaT/src/gambit.scm";
  $guile_exec = "guile --debug";
  $guile_file = "$FermaT/src/ALL.scm";
  $bigloo_exec = "$FermaT/bigloo/$arch/scmfmt";
  $siglist = $Config{sig_name} or die "No signals?";
} else {
  # We are on a brain-dead operating system
  $ds = "\\";
  if (defined($ENV{'TMP'})) {
    $tmpdir = $ENV{'TMP'} . $ds;
  } else {
    $tmpdir = "C:\\TEMP\\";
  }
  $tmpdir = "C:\\TMP\\" unless (-d $tmpdir);
  $tmpdir = "C:\\"      unless (-d $tmpdir);
  $exec = qq["$FermaT\\MinGW\\scmfmt" -p0];
  $siglist = "HUP INT QUIT ILL TRAP ABRT EMT FPE KILL BUS SEGV SYS PIPE ALRM TERM";
}
my $config = "$FermaT${ds}/config";
die "$config directory does not exist!\n" unless -d $config;
unshift(@INC, $config) unless grep { $_ eq $config } @INC;
$perl = "perl";
my $perldir = "$FermaT${ds}perl${ds}$arch";
if (-d $perldir) {
  $perl = qq["$perldir${ds}bin${ds}perl" -I "$perldir${ds}lib" -I "$perldir${ds}site"];
}
my $qperl = $perl;
if (!$dos || ($perl =~ /\s/)) {
  # Put quotes around perl exec if it contains spaces:
  $qperl =~ s#"#" //Quote "#g;
} else {
  # Windows 2000 gets all confused by the quotes:
  $qperl =~ s#"##g;
}
$qperl = qq["$qperl"];
#print STDERR "qperl = <$qperl>\n";

@siglist = split(/\s+/, "$siglist");
unshift(@siglist, "NULL") unless ($siglist[1] eq "HUP");

@goners = ();	# tmp files to delete on interrupt or exit

# Set the scheme environment:
my $init = "";
my $path = "$FermaT${ds}scm${ds}";
opendir(DIR, $path) or die "Can't open directory $path: $!\n";
foreach my $file (readdir(DIR)) {
  if ($file =~ /^Init\d+.\d+\.scm$/) {
    $init = $path . $file;
    last;
  }
}
die "No Init file found in $path!" unless $init;
$ENV{SCM_INIT_PATH}       = $init;
$ENV{SCHEME_LIBRARY_PATH} = "$FermaT${ds}slib${ds}";
$ENV{FermaT}		  = "$FermaT";

# Default values:
$input  = "test.wsl";
$output = "test.out";
$trans = "TR_Simplify";
$data = "";
$extra = "";
$FA_Debug = 0;


sub flip_meta($) {
  my ($str) = @_;
  $str =~ tr/\x00-\xff/\x80-\xff\x00-\x7f/;
  return($str);
}

# Parse command line arguments to overwrite default values:

sub parse_args {
  # Process options
  # DOS bug workaround: arg foo=bar comes out as two args: foo and bar!
  my @args = ();
  while (@ARGV) {
    $opt = shift(@ARGV);
    if ($opt =~ /^(posn|data|exec)$/) {
      $opt .= "=" . shift(@ARGV);
    }
    push(@args, $opt);
  }
  @files = ();
  foreach $opt (@args) {
    if ($exec eq "") {
      $exec = $opt;
      next;
    }
    if ($opt =~ /^type[:=](.*)$/) {
      $type = $1;
      $type = "T_$type" unless ($type =~ /^T_/);
      $type =~ s/[A-Z]/\/$&/g;
      $type = "(\@Find_Type /$type)";
      $extra .= $type
    } elsif ($opt =~ /^posn[:=](.*)$/) {
      $posn = $1;
      $posn =~ s/\(|\)//g;
      $posn =~ s/<|>//g;
      $posn =~ s/,/ /g;
      $extra .= "(\@Goto '($posn))";
    } elsif ($opt =~ /^data[:=](.*)$/) {
      $data = $1;
    } elsif ($opt =~ /^exec[:=](.*)$/) {
      $exec = $1;
    } elsif ($opt eq "-gambit") {
      $ENV{SCM_IMPL} = "gambit";
    } elsif ($opt eq "-scm") {
      $ENV{SCM_IMPL} = "scm";
    } elsif ($opt eq "-guile") {
      $ENV{SCM_IMPL} = "guile";
    } elsif ($opt eq "-bigloo") {
      $ENV{SCM_IMPL} = "bigloo";
    } elsif ($opt eq "-debug") {
      $extra .= "(set! //F/A_/Debug 1)";
      print STDERR "Debugging on...\n";
    } elsif ($opt =~ /^(Simplify|Increment)$/) {
      $trans = "TR_$1";
    } elsif ($opt eq "Increment") {
      $trans = "TR_Increment";
    } elsif (($opt eq "Data_Translation") || ($opt eq "D_T")) {
      $trans = "TR_Data_Translation_A";
    } elsif (($opt eq "X86_Fix") || ($opt eq "x86_Fix")) {
      $trans = "TR_X86_Fix";
    } elsif (($opt eq "Fix_Assembler_X86") || ($opt eq "Fix_Assembler_x86")) {
      $trans = "TR_Fix_Assembler_X86";
    } elsif ($opt eq "C_P") {
      $trans = "TR_Constant_Propagation";
    } elsif ($opt eq "SSA") {
      $trans = "TR_Static_Single_Assignment";
    } elsif (($opt =~ /^TR_/)
	      || ($opt =~ s/^[A-Z]+[a-z]*(_[A-Z][a-z]*)+$/TR_$&/)) {
      $trans = $opt;
      if ($trans =~ /Action_System/) {
	$data = "dispatch";
      }
    } elsif (($opt =~ /\.ll$/) ||
	     ($opt =~ /\.dat$/) ||
	     ($opt eq "dispatch")) {
      $data = $opt;
    } else {
      $opt =~ s/"//g; #"
      push(@files, $opt);
    }
  } # End of options loop
  if (defined($ENV{SCM_IMPL}) && ($ENV{SCM_IMPL} =~ /^(gambit|scm|guile)$/)) {
    # Load the modules required for interaction:
    local $^W = 0;
    require "Expect.pm";
    require "waitfor.pl";
  }
  if ($#files >= 0) {
    $input = $files[0];
    $input =~ s/\.gz$//;
    if (($#files >= 1) && ($0 !~ m#(^|/)wsl$#)) {
      $output = $files[1];
      die "Output file `$output' has no file extension!\n" unless $output =~ /\./;
      if ($#files >= 2) {
	$data = $files[2];
      }
    } else {
      ($output = $input) =~ s/-(\d+)\.wsl$/"-".($1+1).".wsl"/ge
	or ($output = $input) =~ s/\.wsl$/\.out/
	or $output = "$input.out";
    }
  }
  # Give the default data file, if not found:
  if (@files && !$data && ($trans =~ /Data_Translation|WSL_To_C_Pre/)) {
    ($data = $files[0]) =~ s/(-\d+)?(\.wsl)?$//;
    $data .= ".ll";
  }
  # Convert $trans to a Scheme variable:
  $trans =~ s![A-Z]!/$&!g;
  $trans = "/$trans";
  # Uncompress input file if necessary:
  system "gunzip $input.gz" if (!-f $input && -f "$input.gz");
  die "Can't open input file `$input': $!\n" unless (-f $input);

}


# Execute scheme or gambit with the given commands:

sub fermat($) {
  my ($cmds) = @_;
  my $defines = <<END;
  (define fermat "$FermaT")
  (define perl   (string-append $qperl))
  (define ds     "$ds")
END
  if (defined($ENV{Mem_Version}) && ($ENV{Mem_Version} == 1)) {
    $defines .= "  (define //Mem_/Version 1)\n";
  }
  local (*TMP);
  if (!$dos && defined($ENV{SCM_IMPL}) && ($ENV{SCM_IMPL} =~ /^gambit$/)) {
    $cmds = qq[$defines  (include "$gambit_file")\n] . $cmds;
    if (-t STDIN) {
      # STDIN is attached to a tty
      &start_command($gambit_exec);
      &wait_for(">");
      &send_ln($cmds);
      &Interact("\001");
      &send_ln("(exit)");
      &kill_command();
    } else {
      # STDIN not attached to a tty
      # Hack to get gambit to read commands from STDIN:
      open(GAMBIT, qq[|$gambit_exec -e '(include "/dev/stdin")'])
	or die "Can't exec `$gambit_exec': $!\n";
      print GAMBIT $cmds;
      print GAMBIT "(exit)\n";
      close(GAMBIT);
      print "\n";
    }

  } elsif (!$dos && defined($ENV{SCM_IMPL}) && ($ENV{SCM_IMPL} eq "guile")) {
    # Turn on case insensitivity:
    $cmds = qq[(read-enable \'case-insensitive)\n]
             . qq[(debug-set! stack 0)\n]
             . qq[(load "$FermaT/config/macros.scm")\n]
             . qq[  (load "$guile_file")\n$defines] . $cmds;
    if (-t STDIN) {
      # guile with STDIN attached to a tty
      &start_command($guile_exec);
      &wait_for(">");
      &send_ln($cmds);
      &Interact("\001");
      &send_ln("(exit)");
      $timeout = 1;
      &wait_for(">");
      &kill_command();
    } else {
      open(GUILE, qq[|$guile_exec])
	or die "Can't exec `$guile_exec': $!\n";
      print GUILE $cmds;
      print GUILE "(exit)\n";
      close(GUILE);
      print "\n";
    }

  } elsif (defined($ENV{SCM_IMPL}) && ($ENV{SCM_IMPL} eq "bigloo")) {
    # bigloo compiled version
    $defines =~ s/define/set!/g;
    $cmds = <<END;

(define-macro (defmacro name . forms)
  \`(define-macro (,name . ,(car forms)) ,\@(cdr forms)))
(load "$FermaT${ds}config${ds}macros.scm")
(define (get-internal-run-time) 0)
$defines
$cmds
END
    my $tmpfile = tmpfile($cmds);
    mysystem "$bigloo_exec -f $tmpfile";
    unlink($tmpfile);

  } else {
    # Standard Scheme (Control-C defined to cause an exit):
    $cmds = <<END;
(begin
$defines
  (load "$FermaT${ds}config${ds}macros.scm")
$cmds
  (exit))
END
    # Protect \'s in $cmds:
    $cmds =~ s/\\/\\\\/g;
    my $tmpfile = tmpfile($cmds);

    # Delete slibcat only if it is invalid:
    my $slibcat = "$FermaT${ds}scm${ds}slibcat";
    my $ok = 0;
    if (open(SLIBCAT, $slibcat)) {
      local $_;
      while (<SLIBCAT>) {
	next unless /getopt \S+ \"(.*)\"/;
	$ok = 1 if -f "$1.scm";
      }
      close(SLIBCAT); # dos doesn't let you delete an open file!
    }
    if (!$ok) {
      print "Note: slibcat is invalid: recreating file...\n";
      unlink($slibcat);
    }
    mysystem "$exec -f $tmpfile";
    unlink($tmpfile);
  }
}


sub header() {
  if (!$dos) {
    system "date";
    $ls = `ls -alFL $exec`;
    $ls =~ s|/|\n/|;
    print "$ls";
  }
}


# Look in current directory, parent directories or config directory:

sub find_file($) {
  my($file) = @_;
  my($dir);
  return("") if ($file eq "");
  if ($file =~ /\Q$ds\E/) {
    # Assume we have an absolute pathname:
    if (-f $file) {
      return($file);
    } else {
      return("");
    }
  }
  $dir = Cwd::cwd();
  return("$dir${ds}$file") if -f $file;
  # Cwd uses / as dir separator in dos also:
  if (0 && $dos) {
    while ($dir =~ s!\\[^\\]*$!!) {
      return("$dir\\$file") if -f "$dir\\$file";
    }
  } else {
    while ($dir =~ s!/[^/]*$!!) {
      return("$dir${ds}$file") if (-f "$dir${ds}$file");
      return("$dir${ds}config${ds}$file") if (-f "$dir${ds}config${ds}$file");
    }
  }
  return("$FermaT${ds}config${ds}$file") if -f "$FermaT${ds}config${ds}$file";
  return("");
}


sub cwd_old()  {
  my $cwd;
  if ($dos) {
    system "$command_dot_com /c cd > $pathfile";
    print STDERR "$command_dot_com returned $?: $!\n" if ($?);
    open(PATH, $pathfile) or die "Can't open $pathfile: $!\n";
    chomp($cwd = <PATH>);
    close(PATH);
    unlink($pathfile);
    return($cwd);
  } else {
    chomp($cwd = `pwd`);
    return($cwd);
  }
}


sub read_options() {
  my ($optfile, $default);
  # (1) Check for "-o filename" on command line
  # (2) Check for "options" file in current directory
  # (3) Check for "options" file in parent, grandparent etc.
  # (4) Check for environment variable $OPTIONS_FILE
  # (5) Check for $default:
  $default = $::FermaT . $::ds . "config" . $::ds . "options";
  if (($#::ARGV >= 1) && ($::ARGV[0] =~ /^-o$/)) {
    shift(@::ARGV);
    $optfile = shift(@::ARGV);
    die "Options file `$optfile' not found!" unless (-f $optfile);
  } elsif ($optfile = &::find_file("options")) {
    # found options in current dir or an ancestor
  } elsif (defined($ENV{'OPTIONS_FILE'}) && $ENV{'OPTIONS_FILE'}
	     && -f $ENV{'OPTIONS_FILE'}) {
      $optfile = $ENV{'OPTIONS_FILE'};
  } elsif (-f "$default") {
    $optfile = $default;
  } else {
    return(0);
  }
  read_option_file($optfile);
  # Check for an extra options file with overriding options:
  read_option_file("$optfile.001") if (-f "$optfile.001");
  return(1);
}


sub read_option_file($) {
  my ($optfile) = @_;
  my ($name, $value);
  local *OPT;
  # Default options, in case they are not in the file:
  { no strict 'refs';
    foreach my $tag (qw(BIN STR PKD FLO PTR FPR DEF GRP)) {
      ${"::S_COBOL_$tag"} = $tag;
    }
  }
  open(OPT, $optfile) || die "Can't open options file `$optfile': $!\n";
  #print STDERR "Reading options from file: `$optfile'\n";
  while (<OPT>) {
    next if (/^#/);
    next unless (/^\s*(\w+)\s*=\s*(.*)$/);
    $name = $1;
    $value = $2;
    $value =~ s/^\s*(.*?)\s*$/$1/;
    while ($value =~ s/^([^\"]*\"[^\"]*)\\\s*$/$1/) {
      chomp($value);
      die "Bad option value: $value\n" if eof(OPT);
      $value .= <OPT>;
    }
    die "Bad option value: $value\n"
      unless (($value =~ /^\d+$/) || ($value =~ s/^\"([^\"\']*)\"$/$1/));
    # Use a symbolic reference to set the option switch variable:
    { no strict 'refs';
      if (defined(${"::S_$name"}) && (${"::S_$name"} =~ /\s/)) {
	${"::S_$name"} .= " " . $value;
      } else {
        ${"::S_$name"} = $value;
      }
    }
  }
  close(OPT);
  $::tag_pattern = qr/-(?:$::S_COBOL_FPR
                         |(?:$::S_COBOL_DEF|$::S_COBOL_GRP)-\d+
                         |(?:$::S_COBOL_STR|$::S_COBOL_BIN|$::S_COBOL_PKD
		              |$::S_COBOL_FLO|$::S_COBOL_PTR)
                            (?:-\d+-\d+)?)/x;
}


# Run the given WSL file and capture and return all output:
sub run_wsl($$$) {
  my ($opt, $dir, $file) = @_;
  my $line = qq[wsl $opt "$dir${ds}$file"];
  $line .= " < /dev/null" unless $dos;
  $stdout = "";
  mysystem($line, \$stdout);
  return($stdout);
}



# Check if $a (or $a.gz) exists and is newer (or the same age) than $b (or $b.gz)
sub newer($$) {
  my ($a, $b) = @_;
  my ($t_a, $t_b);
  if (!-f $a || (-s _ == 0)) {
    if (-f "$a.gz") {
      $a = "$a.gz";
    } else {
      return (0);
    }
  }
  if (!-f $b || (-s _ == 0)) {
    if (-f "$b.gz") {
      $b = "$b.gz";
    } else {
      die "newer($a, $b) called, but file `$b' does not exist!\n";
    }
  }
  # check mtimes
  $t_a = (stat($a))[9];
  $t_b = (stat($b))[9];
  return ($t_a >= $t_b);
}


# Run a system command saving STDOUT and/or STDERR
# to a file or to the given string ref.
# exec perl directly if the command is in $FermaT/bin

sub mysystem($@) {
  my ($line, $stdout, $stderr) = @_;
  my $orig_line = $line;
  $stdout = "" unless defined($stdout);
  $stderr = $stdout unless defined($stderr);
  my $proc_num = ($$ % 32768);
  my $command = "";
  my $tmp1 = "$tmpdir${ds}mysys1$proc_num";
  my $tmp2 = "$tmpdir${ds}mysys2$proc_num";
  local (*SAVEOUT, *SAVEERR);
  # Check for encrypted perl script:
  if ($line =~ s/^(\"[^\"]+\")\s*//) {
    $command = $1;
  } elsif ($line =~ s/^(\S+)\s*//) {
    $command = $1;
  } else {
    die "mysystem: `$line' has no command name!\n";
  }
  if (!-f "$FermaT${ds}bin${ds}$command" && -f "$FermaT${ds}bin${ds}$command.ep") {
    # Run encrypted perl script:
    die "Cannot execute runcrypt!\n" unless (-x $runcrypt || -x "$runcrypt.exe");
    $command = qq["$runcrypt" "$FermaT${ds}bin${ds}$command.ep" -FermaT "$FermaT"];
  } elsif (-f "$FermaT${ds}bin${ds}$command") {
    # Run perl directly on command and pass FermaT variable and extra lib directory:
    $command = qq[$perl -I "$FermaT${ds}config" ]
		  . qq[ "$FermaT${ds}bin${ds}$command" -FermaT "$FermaT"];
  } elsif ($command =~ /^(\/|[a-zA-Z]:\\|\")/) {
    # Full path name given
  } elsif ($command =~ /^(gcc.*|strip|.*mingw32-gcc|.*mingw32-strip|bigloo|indent|xvcg)$/) {
    # A command that must be in the path somewhere.
  } else {
    # Neither a full path name nor a FermaT/bin script!
    die "Cannot find <$command> <$line>\n";
  }
  $command =~ s/(\Q$ds\E)+\"/\"/g;
  #print STDERR "\n$command $line\n";
  if ($stdout ne "") {
    open(SAVEOUT, ">&STDOUT");
    close(STDOUT);
    if (ref($stdout) eq "SCALAR") {
      open(STDOUT, ">$tmp1") or die "Can't write to $tmp1: $!\n";
      push(@goners, $tmp1);
    } elsif (ref($stdout)) {
      die;
    } else {
      open(STDOUT, ">$stdout") or die "Can't write to $stdout: $!\n";
    }
  }
  if ($stderr ne "") {
    open(SAVEERR, ">&STDERR");
    close(STDERR);
    if ($stderr eq $stdout) {
      open(STDERR, ">&STDOUT");
    } elsif (ref($stderr) eq "SCALAR") {
      open(STDERR, ">$tmp2") or die "Can't write to $tmp2: $!\n";
      push(@goners, $tmp2);
    } elsif (ref($stdout)) {
      die;
    } else {
      open(STDERR, ">$stderr") or die "Can't write to $stderr: $!\n";
    }
  }
  # Protect $ characters in filenames:
  $line =~ s/\$/\\\$/g unless $dos;

  system "$command $line";

  if ($stdout ne "") {
    close(STDOUT);
    open(STDOUT, ">&SAVEOUT");
    close(SAVEOUT);
    if (ref($stdout) eq "SCALAR") {
      local *IN;
      open(IN, $tmp1) or die "Can't read $tmp1: $!\n";
      $$stdout = join("", <IN>);
      close(IN);
      unlink($tmp1);
    }
  }
  if ($stderr ne "") {
    close(STDERR);
    open(STDERR, ">&SAVEERR");
    if ($stderr eq $stdout) {
      # All done
    } elsif (ref($stderr) eq "SCALAR") {
      local *IN;
      open(IN, $tmp2) or die "Can't read $tmp2: $!\n";
      $$stderr = join("", <IN>);
      close(IN);
      unlink($tmp2);
    }
  }

  # Check $? for errors:
  if ($? == -1) {
    warn "$orig_line failed: $!\n";
  } elsif (($? >> 8) == 13) {
    # Control-C interrupt of SCM:
    warn "Interrupt!\n";
    unlink @goners;
    kill "INT", $$;
  } elsif ($?) {
    warn "$orig_line terminated with code: ", ($? >> 8), "\n" if ($? >> 8);
    my $s = $? & 127;
    if ($s) {
      my $sig = $siglist[$s];
      warn "\n$orig_line\n\tdied on SIG$sig($s)\n";
      # Propagate interrupt or hangup signals upwards:
      unlink @goners;
      kill $s, $$ if (($sig eq "INT") || ($sig eq "HUP"));
    }
    warn "Core dumped.\n" if ($? & 128);
    # print last two lines of STDERR if it was redirected:
    if ($stderr ne "") {
      my $lines = "";
      if (ref($stderr) eq "SCALAR") {
	chomp($lines = $$stderr);
      } elsif (-f $stderr) {
	local *LOGFILE;
	open(LOGFILE, $stderr) or die;
	chomp($lines = join("", <LOGFILE>));
      }
      # Trim all but last two lines (if there are more than two)
      $lines =~ s/^.*\n([^\n]*\n[^\n]*)$/$1/;
      print STDERR "\n$lines\n\n" if $lines ne "";
    }
  }
}


# Create a temp file with the given contents
# and return the filename (this is used for "-f filename" args to scm):

$filecount = 1;

sub tmpfile($) {
  my ($data) = @_;
  local *OUT;
  my $proc_num = ($$ % 32768);
  my $name = sprintf("$tmpdir${ds}tmp-$proc_num-%03i.txt", $filecount++);
  push(@goners, $name);
  open(OUT, ">$name") or die "Can't write to $name: $!\n";
  print OUT $data;
  close(OUT);
  return($name);
}


sub cleanup_goners {
  my($sig) = @_;
  unlink(@goners);
  # Restore default behaviour and re-do the signal:
  $SIG{INT} = 'DEFAULT';
  $SIG{HUP} = 'DEFAULT';
  $SIG{QUIT} = 'DEFAULT';
  $SIG{TERM} = 'DEFAULT';
  kill $sig, $$;
}


# Find global variables in a Scheme file:

sub find_globals($$) {
  my ($file, $globals) = @_;
  local($_);
  local(*INFILE);
  open(INFILE, $file) or die "Can't read $file: $!\n";
  while (<INFILE>) {
    next if /^\s*;/;
    $$globals{$1}++ while (s/([^,() "]+)\-save //);
    $$globals{$1}++ while (s/\(set\! ([^,() "]+) //);
    $$globals{$2}++ while (s/\(cons \(([^,()]+)\) ([^() "]+)\)//);
    $$globals{$1}++ while (s/\(for-in ([^,() "]+) //);
    $$globals{$1}++ while (s/\(for ([^,() "]+) //); #"
    $$globals{$1}++ while (s/\(pop ([^,()]+) //);
    $$globals{$1}++ while (s/\(push ([^,()]+) //);
  }
  close(INFILE);
}


# Read COBOL-calls.txt file.
# Format of the file is:
# module number-of-pars
# or:
# module {CICS|CICSEIB|CICSDATA}[,number-of-pars]
# A CICS module takes at least two parameters: the EIB pointer and the COMMAREA pointer
# A CICSEIB module must have an EIB pointer added as a new first parameter
# A CICSDATA module is a data-only CSECT with is preprocessed by CICS:
# -- call the module to get a pointer to the data via the COMMAREA parameter.
#
# or:
# module number-of-pars,RRn,...
# or:
# module Rn,...[,RRm,...]
#   where Rn are registers passed as parameters
#   (the parameter's address is set to the value of the register)
#   and RRm are registers which are returned as parameters.
# or:
# module (par1,par2,...,Rn=parn,...)
#   where parn are the actual parameters
#   and Rn=parn means that register Rn should be initialised with
#   the address of parn (i.e. the assembler passed this address
#   via this register)
#

sub read_calls_file($) {
  my ($file) = @_;
  return() unless $file;
  local($_);
  local(*IN);
  my %calls;
  open(IN, $file) or die "Can't read $file: $!\n";
  while (<IN>) {
    next if /^\#/;
    next if /^\s*$/;
    chomp;
    while (s/\\$//) {
      die if eof(IN);
      my $cont = <IN>;
      $cont =~ s/^\s+//;
      chomp($cont);
      $_ .= $cont;
    }
    if (/^(\S+)\s+(\(\S+\))(\s+.*)?$/) {
      my $name = $1;
      my $pars = $2;
      $calls{lc($name)} = $pars;
      $calls{lc($name)} = $pars if $name =~ s/^.*\.(.*)$/$1/;
    } else {
      die "Unexpected line in $file:\n$_\n"
        unless /^(\S+)\s+((\d+|[A-Z]\S*)(,RR?\d+)*)(\s+.*)?$/;
      my $name = $1;
      my $pars = $2;
      $calls{lc($name)} = $pars;
      $calls{lc($name)} = $pars if $name =~ s/^.*\.(.*)$/$1/;
    }
  }
  close(IN);
  return(%calls);
}


# Check for FermaT_parallel environment variable
# and run given script in parallel with given arguments:

sub parallel_check($@) {
  my ($script, @args) = @_;
  my $dir = Cwd::cwd();
  if (defined($ENV{HOST}) && ($ENV{HOST} eq "pingu6")) {
    $dir =~ s#^/mnt/server##;
    if ($dir =~ m#^/shared/#) {
      $script = "--sshlogin 16/server,8/pingu5,16/: $script -cdir $dir";
    }
  }
  if ($ENV{FermaT_parallel} && @args > 1) {
    if (($ENV{FermaT_parallel} eq "1") && ($dir !~ m#^/shared/#)) {
      $ENV{FermaT_parallel} = 3;
    }
    if ($ENV{FermaT_parallel} eq "1") {
      print "  =====  Checking pingu5 ...  =====  \n";
      my $r = `ping -c1 pingu5`;
      print $r;
      if ($r =~ /1 received/) {
        $ENV{FermaT_parallel} = 2;
      } else {
        $ENV{FermaT_parallel} = 3;
      }
    }
    if ($ENV{FermaT_parallel} eq "3") {
      # pingu5 is not running
      $script =~ s#,8/pingu5##;
    }
    open(PAR, "|parallel $script") or die "Cannot exec parallel: $!\n";
    # Sort @args so that the largest files are processed first
    if (-f $args[0]) {
      @args = map { $_->[0] }
	      reverse
              sort { $a->[1] <=> $b->[1] }
              map { [$_, -s] } @args;
    } elsif (-f ($args[0] . ".lst")) {
      @args = map { $_->[0] } 
	      reverse
              sort { $a->[1] <=> $b->[1] }
              map { [$_, -s "$_.lst"] } @args;
    }
    print PAR map { "$_\n" } @args;
    close(PAR);
    exit(0);
  }
}


# Read the given CSV file and return a list of lists:

sub read_csv($) {
  my ($file) = @_;
  my @rows = ();
  local($_);
  open(my $in, '<', $file) or die "Cannot open $file: $!\n";
  while (<$in>) {
    chomp;
    # Skip blank lines:
    next unless /\S/;
    # Skip commented out lines:
    next if /^#\"/;
    my @cols = ();
    while ($_ =~ /\S/) {
      s/^\s+//;
      1 while s/^(\"[^"]*)\"\"/$1\'/;
      if (s/^\"([^"]*)\"//) {
	push(@cols, $1);
      } elsif (s/^([^,]*)//) {
	push(@cols, $1);
      } elsif (/^\s*,/) {
	push(@cols, "");
      } else {
	die "Cannot parse csv line $. in file $file:\n$_\n";
      }
      last unless /\S/;
      s/^\s*,// or die "Badly formatted csv line in $file:\n$_\n";
    }
    push(@rows, \@cols);
  }
  close($in);
  return(\@rows);
}


# Write the given CSV data to the given filename:

sub write_csv($$) {
  my ($file, $data) = @_;
  die "Data passed to write_csv($file,...) is not an array ref!\n"
    unless ref($data) eq "ARRAY";
  open(my $out, '>', $file) or die "Cannot write to $file: $!\n";
  foreach my $line (@$data) {
    if ($line eq "") {
      print $out "\n";
      next;
    }
    die "Line in data passed to write_csv($file,...) is not an array ref!\n"
      unless ref($line) eq "ARRAY";
    my $comma = 0;
    foreach my $item (@$line) {
      next unless defined($item);
      print $out "," if $comma++;
      if (ref($item) eq "HASH") {
	print $out '"' . join(",", sort keys %$item) . '"';
      } elsif (ref($item)) {
	die "Unexpected ref ($item) in write_csv($file,...)\n";
      } elsif ($item =~ /^\d+$/) {
	print $out $item;
      } elsif ($item eq "") {
	# empty item
      } else {
	print $out qq["$item"];
      }
    }
    print $out "\n";
  }
  close($out)
}


# NB: "dd conv=ebcdic" and "dd conv=ascii" are incorrect re IBM
# "dd conv=ibm" seems correct for ASCII to IBM EBCDIC,
# but there does not appear to be a reverse translation!

# "|" vertical bar is 0x7C in ASCII and 0x4F in EBCDIC
# "¦" broken bar   is 0xA6 in ASCII and 0x6A in EBCDIC
# "!" exclamation  is 0x21 in ASCII and 0x5A in EBCDIC

# A string containing all ASCII characters in EBCDIC order:
# (for converting EBCDIC to ASCII):
$ascii = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F" .
         "\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1A\x1B\x1C\x1D\x1E\x1F" .
         "\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2A\x2B\x2C\x2D\x2E\x2F" .
         "\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\x3A\x3B\x3C\x3D\x3E\x3F" .
         "\x20\x41\x42\x43\x44\x45\x46\x47\x48\x49\x4A\x2E\x3C\x28\x2B\x7C" .
         "\x26\x51\x52\x53\x54\x55\x56\x57\x58\x59\x21\x24\x2A\x29\x3B\xAC" .
         "\x2D\x2F\x62\x63\x64\x65\x66\x67\x68\x69\xA6\x2C\x25\x5F\x3E\x3F" .
         "\x70\x71\x72\x73\x74\x75\x76\x77\x78\x60\x3A\x23\x40\x27\x3D\x22" .
         "\x80\x61\x62\x63\x64\x65\x66\x67\x68\x69\x8A\x7B\x8C\x8D\x8E\x8F" .
         "\x90\x6A\x6B\x6C\x6D\x6E\x6F\x70\x71\x72\x9A\x9B\x9C\x9D\x9E\x9F" .
         "\xA0\x7E\x73\x74\x75\x76\x77\x78\x79\x7A\xAA\xAB\xAC\x5B\xAE\xAF" .
         "\xB0\xB1\xB2\xB3\xB4\xB5\xB6\xB7\xB8\xB9\xBA\xBB\xBC\x5D\xBE\xBF" .
         "\x7B\x41\x42\x43\x44\x45\x46\x47\x48\x49\xCA\xCB\xCC\xCD\xCE\xCF" .
         "\x7D\x4A\x4B\x4C\x4D\x4E\x4F\x50\x51\x52\xDA\xDB\xDC\xDD\xDE\xDF" .
         "\x5C\xE1\x53\x54\x55\x56\x57\x58\x59\x5A\xEA\xEB\xEC\xED\xEE\xEF" .
         "\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\xFA\xFB\xFC\xFD\xFE\xFF";

# A string containing all EBCDIC characters in ASCII order:
# (for converting ASCII to EBCDIC)
$ebcdic = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F" .
          "\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1A\x1B\x1C\x1D\x1E\x1F" .
          "\x40\x5A\x7F\x7B\x5B\x6C\x50\x7D\x4D\x5D\x5C\x4E\x6B\x60\x4B\x61" .
          "\xF0\xF1\xF2\xF3\xF4\xF5\xF6\xF7\xF8\xF9\x7A\x5E\x4C\x7E\x6E\x6F" .
          "\x7C\xC1\xC2\xC3\xC4\xC5\xC6\xC7\xC8\xC9\xD1\xD2\xD3\xD4\xD5\xD6" .
          "\xD7\xD8\xD9\xE2\xE3\xE4\xE5\xE6\xE7\xE8\xE9\xAD\xE0\xBD\x5E\x6D" .
          "\x79\x81\x82\x83\x84\x85\x86\x87\x88\x89\x91\x92\x93\x94\x95\x96" .
          "\x97\x98\x99\xA2\xA3\xA4\xA5\xA6\xA7\xA8\xA9\xC0\x4F\xD0\xA1\x7F" .
          "\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8A\x8B\x8C\x8D\x8E\x8F" .
          "\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9A\x9B\x9C\x9D\x9E\x9F" .
          "\xA0\xA1\xA2\xA3\xA4\xA5\x6A\xA7\xA8\xA9\xAA\xAB\x5F\xAD\xAE\xAF" .
          "\xB0\xB1\xB2\xB3\xB4\xB5\xB6\xB7\xB8\xB9\xBA\xBB\xBC\xBD\xBE\xBF" .
          "\xC0\xC1\xC2\xC3\xC4\xC5\xC6\xC7\xC8\xC9\xCA\xCB\xCC\xCD\xCE\xCF" .
          "\xD0\xD1\xD2\xD3\xD4\xD5\xD6\xD7\xD8\xD9\xDA\xDB\xDC\xDD\xDE\xDF" .
          "\xE0\xE1\xE2\xE3\xE4\xE5\xE6\xE7\xE8\xE9\xEA\xEB\xEC\xED\xEE\xEF" .
          "\xF0\xF1\xF2\xF3\xF4\xF5\xF6\xF7\xF8\xF9\xFA\xFB\xFC\xFD\xFE\xFF";

$ebcdic = $ebcdic; # Prevent a warning
$ascii = $ascii;   # Prevent a warning

# IBM 1140 tables (see print-ebcdic-1140-tables):

$ascii = "\x00\x01\x02\x03\x9C\x09\x86\x7F\x97\x8D\x8E\x0B\x0C\x0D\x0E\x0F" .
         "\x10\x11\x12\x13\x9D\x85\x08\x87\x18\x19\x92\x8F\x1C\x1D\x1E\x1F" .
         "\x80\x81\x82\x83\x84\x0A\x17\x1B\x88\x89\x8A\x8B\x8C\x05\x06\x07" .
         "\x90\x91\x16\x93\x94\x95\x96\x04\x98\x99\x9A\x9B\x14\x15\x9E\x1A" .
         "\x20\xA0\xE2\xE4\xE0\xE1\xE3\xE5\xE7\xF1\xA2\x2E\x3C\x28\x2B\x7C" .
         "\x26\xE9\xEA\xEB\xE8\xED\xEE\xEF\xEC\xDF\x21\x24\x2A\x29\x3B\xAC" .
         "\x2D\x2F\xC2\xC4\xC0\xC1\xC3\xC5\xC7\xD1\xA6\x2C\x25\x5F\x3E\x3F" .
         "\xF8\xC9\xCA\xCB\xC8\xCD\xCE\xCF\xCC\x60\x3A\x23\x40\x27\x3D\x22" .
         "\xD8\x61\x62\x63\x64\x65\x66\x67\x68\x69\xAB\xBB\xF0\xFD\xFE\xB1" .
         "\xB0\x6A\x6B\x6C\x6D\x6E\x6F\x70\x71\x72\xAA\xBA\xE6\xB8\xC6\x80" .
         "\xB5\x7E\x73\x74\x75\x76\x77\x78\x79\x7A\xA1\xBF\xD0\xDD\xDE\xAE" .
         "\x5E\xA3\xA5\xB7\xA9\xA7\xB6\xBC\xBD\xBE\x5B\x5D\xAF\xA8\xB4\xD7" .
         "\x7B\x41\x42\x43\x44\x45\x46\x47\x48\x49\xAD\xF4\xF6\xF2\xF3\xF5" .
         "\x7D\x4A\x4B\x4C\x4D\x4E\x4F\x50\x51\x52\xB9\xFB\xFC\xF9\xFA\xFF" .
         "\x5C\xF7\x53\x54\x55\x56\x57\x58\x59\x5A\xB2\xD4\xD6\xD2\xD3\xD5" .
         "\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\xB3\xDB\xDC\xD9\xDA\x9F";

$ebcdic = "\x00\x01\x02\x03\x37\x2D\x2E\x2F\x16\x05\x25\x0B\x0C\x0D\x0E\x0F" .
          "\x10\x11\x12\x13\x3C\x3D\x32\x26\x18\x19\x3F\x27\x1C\x1D\x1E\x1F" .
          "\x40\x5A\x7F\x7B\x5B\x6C\x50\x7D\x4D\x5D\x5C\x4E\x6B\x60\x4B\x61" .
          "\xF0\xF1\xF2\xF3\xF4\xF5\xF6\xF7\xF8\xF9\x7A\x5E\x4C\x7E\x6E\x6F" .
          "\x7C\xC1\xC2\xC3\xC4\xC5\xC6\xC7\xC8\xC9\xD1\xD2\xD3\xD4\xD5\xD6" .
          "\xD7\xD8\xD9\xE2\xE3\xE4\xE5\xE6\xE7\xE8\xE9\xBA\xE0\xBB\xB0\x6D" .
          "\x79\x81\x82\x83\x84\x85\x86\x87\x88\x89\x91\x92\x93\x94\x95\x96" .
          "\x97\x98\x99\xA2\xA3\xA4\xA5\xA6\xA7\xA8\xA9\xC0\x4F\xD0\xA1\x07" .
          "\x9F\x21\x22\x23\x24\x15\x06\x17\x28\x29\x2A\x2B\x2C\x09\x0A\x1B" .
          "\x30\x31\x1A\x33\x34\x35\x36\x08\x38\x39\x3A\x3B\x04\x14\x3E\xFF" .
          "\x41\xAA\x4A\xB1\x9F\xB2\x6A\xB5\xBD\xB4\x9A\x8A\x5F\xCA\xAF\xBC" .
          "\x90\x8F\xEA\xFA\xBE\xA0\xB6\xB3\x9D\xDA\x9B\x8B\xB7\xB8\xB9\xAB" .
          "\x64\x65\x62\x66\x63\x67\x9E\x68\x74\x71\x72\x73\x78\x75\x76\x77" .
          "\xAC\x69\xED\xEE\xEB\xEF\xEC\xBF\x80\xFD\xFE\xFB\xFC\xAD\xAE\x59" .
          "\x44\x45\x42\x46\x43\x47\x9C\x48\x54\x51\x52\x53\x58\x55\x56\x57" .
          "\x8C\x49\xCD\xCE\xCB\xCF\xCC\xE1\x70\xDD\xDE\xDB\xDC\x8D\x8E\xDF";


END {
  unlink(@goners);
}

1;

