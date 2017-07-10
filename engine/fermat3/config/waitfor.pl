# RCS $Id: waitfor.pl,v 1.8 2005/06/29 14:38:14 martin Exp $

# Default timeout:
$timeout = 30;

# Get a password from the terminal (Don't echo it!):
sub get_password {
  local($password);
  local($line);
  my $n;
  if (@_) {
    ($n) = @_;
  } else {
    $n = 1;
  }
  # Check ~/.fetchmailrc first:
  my $HOME = $ENV{'HOME'} || $ENV{'LOGDIR'} ||
		(getpwuid($<))[7] || die "You're homeless!\n";
  if (open(PW, "$HOME/.fetchmailrc")) {
    while ($line = <PW>) {
      if ($line =~ /password\s+(\S+)/) {
	return($1) if ($n == 1);
	$n--;
      }
    }
  }
  open(TERM,"/dev/tty") || return("anonymous"); # no terminal
  system "stty -echo";
  select(TERM);$|=1;select(STDOUT);
  print "Password?\n";
  $password = <TERM>;
  chop($password);
  system "stty echo pass8";
  return($password);
}

# Ping a remote host in order to ensure the ppp link is established:
sub ping_link {
  $| = 1;
  print "Pinging... ";
  my $host = $ENV{PING_LINK_HOST} || 'www.dur.ac.uk';
  my $res = `/usr/sbin/ping $host 60`;
  if ($res =~ /alive/) {
    print "OK\n\r";
  } else {
    print $res;
  }
}




# start the command:
sub start_command {
  my ($command) = @_;
  $Proc = Expect->spawn(split(/\s+/, $command));
  #system "stty erase '^H' < " . $Proc->IO::Pty::ttyname() . " &";
}


# wait_pat(pattern ...) -- Eat chars until pattern is matched.
# return the pattern which matched

sub wait_pat {
  my (@patterns) = @_;
  my @strings = ();
  for my $pat (@patterns) {
    push(@strings, "-re", $pat);
  }
  return(wait_for(@strings));
}


# wait_for(string ...) -- Eat chars until a string is matched.
# return the string which matched

sub wait_for {
  my (@patterns) = @_;
  print "Waiting for: @patterns\n" if ($debug);
  my ($pos, $err, $match, $before, $after) = $Proc->expect($timeout, @patterns);
  print "Result = \n--$pos\n--$match\n--$before\n--$after\n" if ($debug);
  if (defined($err)) {
    if ($err eq "1:TIMEOUT") {
      die "Timed out.\n";
    } else {
      die $err;
    }
  }
  return($patterns[$pos - 1]);
}


# Switch to interactive mode -- take input from stdin:
# No timeout.
sub Interact {
  return unless (-t);
  local($escape) = @_;
  print "\r\n### begin interactive mode ###\r\n";
  $Proc->interact(\*STDIN, $escape);
  print "\n### end interactive mode ###\n";
}

 
# Disconnect

sub quit_command {
  print "quit\n";
  return unless ($Proc->pid());
  kill(HUP, $Proc->pid());
}

sub kill_command {
  return unless defined($Proc);
  return unless ($Proc->pid());
  my $kills = kill(HUP, $Proc->pid());
  print "...already dead.\n" unless ($kills);
}


# send a string to command:

sub send {
  my ($send) = @_;
  print "$send" if ($logging);
  print $Proc "$send";
}

# send a line command telnet:

sub send_ln {
  my ($send) = @_;
  print "$send\n" if ($logging);
  print $Proc "$send\n";
}

1;
