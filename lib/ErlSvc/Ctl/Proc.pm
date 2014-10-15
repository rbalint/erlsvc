#-
# Copyright 2011 Yakaz. All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 
#    1. Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
# 
#    2. Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY YAKAZ ``AS IS'' AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL YAKAZ OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
# OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
# BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
# OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
# EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

package ErlSvc::Ctl::Proc;

use strict;
use warnings;
use utf8;

use BSD::Resource;
use Config;
use IO::Handle;
use POSIX qw(setuid setgid :sys_wait_h);
use Socket;
use User::grent;
use User::pwent;

our %SYSEXITS = (
    'EX_OK'          =>  0,
    'EX_USAGE'       => 64,
    'EX_DATAERR'     => 65,
    'EX_NOINPUT'     => 66,
    'EX_NOUSER'      => 67,
    'EX_NOHOST'      => 68,
    'EX_UNAVAILABLE' => 69,
    'EX_SOFTWARE'    => 70,
    'EX_OSERR'       => 71,
    'EX_OSFILE'      => 72,
    'EX_CANTCREAT'   => 73,
    'EX_IOERR'       => 74,
    'EX_TEMPFAIL'    => 75,
    'EX_PROTOCOL'    => 76,
    'EX_NOPERM'      => 77,
    'EX_CONFIG'      => 78
);

our %SYSEXITS_REVERSED = map { $SYSEXITS{$_} => $_; } keys %SYSEXITS;

sub new ($) {
    my ($class, $app) = @_;

    my $self = {
        'reopen_stdio' => 1,
        'env'          => {},
        'app'          => $app
    };

    bless $self => $class;
}

sub run_as_user ($;$) {
    my ($self, $user, $group) = @_;

    if ($user) {
        $self->{'user'} = $user;
    } else {
        delete $self->{'user'};
    }

    if ($group) {
        $self->{'group'} = $group;
    } else {
        delete $self->{'group'};
    }
}

sub set_open_files_limit ($) {
    my ($self, $limit) = @_;

    $self->{'open_files_limit'} = $limit;
}

sub reopen_stdio ($) {
    my ($self, $reopen) = @_;

    $self->{'reopen_stdio'} = $reopen;
}

sub set_env (%) {
    my ($self, %export_env) = @_;

    $self->{'env'} = \%export_env;
}

sub add_env (%) {
    my ($self, %export_env) = @_;

    while (my ($key, $value) = each %export_env) {
        $self->{'env'}->{$key} = $value;
    }
}

sub working_dir () {
    my ($self) = @_;

    my $wd = $self->{'working_dir'};
    unless ($wd) {
        if (defined $self->{'user'}) {
            $wd = $self->app->home($self->{'user'});
        } else {
            return;
        }
    }

    return $wd;
}

sub set_working_dir ($) {
    my ($self, $wd) = @_;

    unless (-d $wd) {
        $self->app->log->error(
            "Problem:\n",
            "  Working directory '$wd' isn't a directory. It'll be ignored.",
            "  System reports:",
            "    $!");
        return;
    }

    $self->{'working_dir'} = $wd;
}

sub pid () {
    my ($self) = @_;

    return $self->{'pid'};
}

sub fh () {
    my ($self) = @_;

    return $self->{'fh'};
}

sub start (@) {
    my ($self, @cmdline) = @_;

    # We use fork() to spawn a process to execute the script. We use
    # this method instead of the simple backticks to get a chance to
    # execute some code, like changing (dropping) process privileges,
    # before executing the real command.

    # We communicate with the child process using a Unix socket. We
    # ignore SIGPIPE here and let any read handle the error.
    my ($child_fh, $parent_fh);
    $SIG{'PIPE'} = 'IGNORE';
    socketpair($child_fh, $parent_fh, AF_UNIX, SOCK_STREAM, PF_UNSPEC)
        or return;

    $child_fh->autoflush(1);
    $parent_fh->autoflush(1);

    # Setup a handler for SIGINT.
    $SIG{'INT'} = \&_sighandler_SIGINT;

    # We're ready to fork!
    $self->app->log->debug("PROC", "Fork the child process\n");
    my $pid = fork();

    if ($pid == 0) {
        # In the CHILD process.
        close $child_fh;

        unless ($self->{'reopen_stdio'}) {
            # Backup original STDIN and STDOUT.
            my ($oldin, $oldout);
            open($oldin,  '<&STDIN')  or return;
            open($oldout, '>&STDOUT') or return;
            $self->{'oldin'}  = $oldin;
            $self->{'oldout'} = $oldout;
        }

        # Reopen STDIN and STDOUT to use the Unix socket.
        open(STDIN,  '<&', $parent_fh) or return;
        open(STDOUT, '>&', $parent_fh) or return;

        # Continue with child specific code.
        $self->_child_run(@cmdline);
    } elsif (defined $pid) {
        # In the PARENT process.
        close $parent_fh;

        $self->app->log->debug("PROC", "Child process PID: $pid\n");

        while (<$child_fh>) {
            chomp;
            my $line = $_;

            if ($line eq 'CTL EXEC') {
                $self->app->log->debug("PROC",
                    "Child: exec $cmdline[0]\n");
                last;
            } else {
                $self->interpret_generic_output($line);
            }
        }

        # Keep child's PID and file handle (to communicate).
        $self->{'pid'} = $pid;
        $self->{'fh'}  = $child_fh;
        return 1;
    }
}

sub _child_run (@) {
    my ($self, @cmdline) = @_;

    # Because this function runs in the child process, the return value
    # isn't relevant. Instead we communicate with the parent process
    # through STDOUT. The private functions _report_* take care of this.

    # We want core dump!
    $self->_report_debug("setrlimit: Enable core dumps");
    my $ret = setrlimit(RLIMIT_CORE, RLIM_INFINITY, RLIM_INFINITY);
    unless ($ret) {
        $self->_report_warning(
            "Problem:",
            "  Failed to enable core dump:",
            "  System reports:",
            "    $!");
    }

    # Raise open files limit.
    my $nofile = $self->{'open_files_limit'};
    if ($nofile) {
        $self->_report_debug(
            "setrlimit: Raise file descriptors limit to $nofile");
        $ret = setrlimit(RLIMIT_NOFILE, $nofile, $nofile);
        unless ($ret) {
	    (my $nowsoft, my $nowhard) = getrlimit(RLIMIT_NOFILE);
	    $ret = setrlimit(RLIMIT_NOFILE, $nowhard, $nowhard);
	    if ($ret) {
		$self->_report_warning(
		    "Problem:",
		    "  Failed to raise the file descriptors limit to $nofile.",
		    "  Raised to $nowhard (hard limit) instead.");
	    } else {
		$self->_report_warning(
		    "Problem:",
		    "  Failed to raise the file descriptors limit to $nofile:",
		    "  System reports:",
		    "    $!");
	    }
	}
    }

    # We change user and group ID.
    my $user  = $self->{'user'};
    my $group = $self->{'group'};

    # We change the group first, because we may not have the
    # permission to do it once the user has changed.
    if ($group && $self->app->group_changed($group)) {
        my $gid = $self->app->gid($group);
        unless (defined $gid) {
            $self->_report_exception(
                "Failed to get GID for group $group:", $!);
            exit($SYSEXITS{'EX_NOUSER'});
        }

        $self->_report_debug("Change process GID to '$group' ($gid)");

        my $ret = setgid($gid);
        unless ($ret) {
            $self->_report_exception(
                "Failed to set process GID to $group ($gid):", $!);
            exit($SYSEXITS{'EX_NOPERM'});
        }
    }
    if ($user && $self->app->user_changed($user)) {
        my $uid = $self->app->uid($user);
        unless (defined $uid) {
            $self->_report_exception(
                "Failed to get UID for user $user:", $!);
            exit($SYSEXITS{'EX_NOUSER'});
        }

        $self->_report_debug("Change process UID to '$user' ($uid)");

        my $ret = setuid($uid);
        unless ($ret) {
            $self->_report_exception(
                "Failed to set process UID to $user ($uid):", $!);
            exit($SYSEXITS{'EX_NOPERM'});
        }

        # We need to update $HOME to point to the effective user home
        # directory, not the original user one.
        $ENV{'HOME'} = $self->app->home($user);
        $self->_report_debug('Set $HOME to \''.$ENV{'HOME'}."\'");
    }

    if (keys %{$self->{'env'}}) {
        $self->_report_debug("Export environment variables:");
        foreach my $var (keys %{$self->{'env'}}) {
            my $value = $self->{'env'}->{$var};
            $self->_report_debug(" $var = $value");
            $ENV{$var} = $value;
        }
    }

    # Change the working directory to the given directory or, if a user
    # was specified, to its home directory.
    my $wd = $self->working_dir;
    if ($wd) {
        $ret = chdir($wd);
        if ($ret) {
            $self->_report_debug(
                "Changed working directory to '$wd'");
        } else {
            $self->_report_warning(
                "Problem:\n",
                "  Failed to change working directory to '$wd'",
                "  System reports:",
                "    $!");
        }
    }

    # Tell the parent that we're about to exec().
    $self->_report_exec;

    unless ($self->{'reopen_stdio'}) {
        # Restore STDIN and STDOUT.
        $self->_report_debug("Restore STDIN and STDOUT");
        open(STDIN,  '<&', $self->{'oldin'});
        open(STDOUT, '>&', $self->{'oldout'});
    }

    # Ready to run the script.
    exec { $cmdline[0] } @cmdline;
}

sub _report_warning (@) {
    my ($self, @message) = @_;

    foreach my $line (@message) {
        print "CTL LOG WARNING $line\n";
    }
}

sub _report_debug (@) {
    my ($self, @message) = @_;

    foreach my $line (@message) {
        print "CTL LOG PROC $line\n";
    }
}

sub _report_exception (@) {
    my ($self, @reason) = @_;

    print << "EOF";
status: exception:
reason: |
EOF
    foreach my $line (@reason) {
        print "  $line\n";
    }
}

sub _report_exec () {
    print "CTL EXEC\n";
}

sub interpret_generic_output ($) {
    my ($self, $line) = @_;

    if ($line =~ /^CTL LOG ([A-Z]+) (.*)$/o) {
        # Something to log.
        my $loglevel   = $1;
        my $message    = $2;

        # Check if the message should end with a newline character.
        my $newline = 1;
        if ($message =~ / NONL$/o) {
            $message =~ s/ NONL$//o;
            $newline = 0;
        }

        if ($loglevel eq 'ERROR') {
            $self->app->log->error(
                $message.($newline ? "\n" : ''));
        } elsif ($loglevel eq 'WARNING') {
            $self->app->log->warning(
                $message.($newline ? "\n" : ''));
        } elsif ($loglevel eq 'INFO') {
            $self->app->log->info(
                $message.($newline ? "\n" : ''));
        } else {
            # $loglevel contains the facility's name here.
            $self->app->log->debug($loglevel,
                $message.($newline ? "\n" : ''));
        }
    } elsif ($line =~ /^CTL WAITING (START|STOP)$/o) {
        if ($1 eq 'START') {
            # The script is waiting for something. Thanks to this
            # notification, we can let the user know through some
            # animation that the program didn't crashed.
            $self->app->log->waiting(1);
        } else {
            $self->app->log->waiting(0);
        }
    } else {
        $self->app->log->debug('PROC',
            "Unexpected output: $line\n");
    }
}

sub wait_for_child () {
    my ($self) = @_;

    my $pid = $self->pid;
    return unless $pid;
    delete $self->{'pid'};

    my $child_fh = $self->fh;
    delete $self->{'fh'};

    # Read remaining data from child process.
    while (<$child_fh>) {
        chomp;
        my $line = $_;

        # Interpret Erlang output.
        $self->interpret_generic_output($line);
    }

    close($child_fh);

    # Get the process exit status.
    $self->app->log->debug("PROC",
        "Wait for child process ($pid) to exit\n");
    waitpid($pid, 0);
    my $exit_code = $?;

    my $interrupted = WIFSIGNALED($exit_code);
    if ($interrupted and $self->app->log->is_waiting) {
        $self->app->log->info(" (interrupted)\n");
    }
    $exit_code = WEXITSTATUS($exit_code);

    my $exit_code_name = $SYSEXITS_REVERSED{$exit_code};
    if ($exit_code_name) {
        $self->app->log->debug("PROC",
            "Process $pid exit code: $exit_code_name ($exit_code)".
            ($interrupted ? ' (interrupted)' : '')."\n");
    } else {
        $self->app->log->debug("PROC",
            "Process $pid exit code: $exit_code".
            ($interrupted ? ' (interrupted)' : '')."\n");
    }

    return ($interrupted) ? -1 : $exit_code;
}

sub sysexit_name ($) {
    my ($self, $code) = @_;

    return $SYSEXITS_REVERSED{$code};
}

sub sysexit_code ($) {
    my ($self, $name) = @_;

    return $SYSEXITS{$name};
}

sub _sighandler_SIGINT {}

sub app ()  { shift->{'app'}; }

1;
