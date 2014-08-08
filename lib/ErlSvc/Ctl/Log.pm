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

package ErlSvc::Ctl::Log;

use strict;
use warnings;
use utf8;

use POSIX qw(strftime);
use Sys::Syslog qw(:standard :macros);
use Time::HiRes qw (setitimer ITIMER_REAL);
use ErlSvc::Ctl::Usage qw(progname);

our $SPIN_INTV = 0.03;
our @SPIN = (
    ' /',
    ' -',
    ' \\',
    ' |'
);

sub new ($) {
    my ($class, $app) = @_;
    $class = ref($class) || $class;

    # Use tput(1) to obtain the number of columns for this terminal. If
    # $TERM is not defined (tput(1) will fail), assume a default of 80
    # columns. This is the case when running erlsvc through ssh(1),
    # like this:
    #   ssh user@host erlsvc status.
    my $columns = $ENV{'TERM'} ? `tput cols` + 0 : 80;

    my $self = {
        'updating'         => 0,
        'waiting'          => 0,
        'unfinished_lines' => [],
        'unfinished_ctx'   => {},
        'spinning'         => 0,
        'spin_frame'       => 0,
        'columns'          => $columns,
        'app'              => $app
    };

    bless $self => $class;

    # Hide cursor.
    $self->show_cursor(0);

    # Setup syslog.
    my $ident    = progname();
    my $logopts  = 'pid';
    my $facility = $self->app->opts('syslog_facility') || 'LOG_USER';
    openlog($ident, $logopts, $facility);

    return $self;
}

sub DESTROY {
    my ($self) = @_;

    # Stop spinning.
    $self->waiting(0);

    # Terminate unfinished lines.
    if (scalar @{$self->{'unfinished_lines'}}) {
        print STDERR "\n";
    }

    # Close syslog.
    closelog();

    # Unhide cursor.
    $self->show_cursor(1);
}

sub error {
    my $self = shift;

    my $color_start = "\e[31m";
    my $color_end   = "\e[0m";

    my $prefix = ($self->app->verbose) ? $self->timestamp : '';

    my @expanded = $self->_expand_lines(@_);

    $self->_syslog('ERROR', @expanded);
    $self->_log('ERROR', $prefix, $color_start, $color_end, @expanded);
}

sub warning {
    my $self = shift;

    my $color_start = "\e[33m";
    my $color_end   = "\e[0m";

    my $prefix = ($self->app->verbose) ? $self->timestamp : '';

    my @expanded = $self->_expand_lines(@_);

    $self->_syslog('WARNING', @expanded);
    $self->_log('WARNING', $prefix, $color_start, $color_end, @expanded);
}

sub info {
    my $self = shift;

    my $prefix = ($self->app->verbose) ? $self->timestamp : '';

    my @expanded = $self->_expand_lines(@_);

    $self->_syslog('INFO', @expanded);
    $self->_log('INFO', $prefix, '', '', @expanded);
}

sub debug {
    my $self      = shift;
    my $component = shift;

    my @expanded = $self->_expand_lines(@_);

    $self->_syslog($component, @expanded);

    return unless ($self->app->verbose($component));

    $self->_log($component, $self->timestamp, '', '', @expanded);
}

sub timestamp () {
    strftime("[%Y-%m-%d %H:%M:%S] ", localtime);
}

sub _expand_lines (@) {
    my ($self, @lines) = @_;

    my @expanded = ();
    foreach my $line (@lines) {
        if (ref($line) eq 'CODE') {
            my @sublines = $self->_expand_lines($line->());
            push @expanded, @sublines;
        } else {
            push @expanded, $line;
        }
    }

    return @expanded;
}

sub _log ($$$$@) {
    my ($self, $context, $prefix, $color_start, $color_end, @lines) = @_;

    return unless (scalar @lines > 0);

    $self->{'updating'} = 1;

    foreach my $line (@lines) {
        $self->_log_line($context, $prefix, $color_start, $color_end, $line);
    }

    $self->{'spinning'} = 0;
    $self->{'updating'} = 0;
}

sub _log_line ($$$$$) {
    my ($self, $context, $prefix, $color_start, $color_end, $line) = @_;

    my $unfinished_count = scalar @{$self->{'unfinished_lines'}};
    my $columns = $self->{'columns'};
    my $unfinished_rows_count = 0;
    if ($columns > 0) {
        foreach my $ul (@{$self->{'unfinished_lines'}}) {
            $unfinished_rows_count +=
              int((length($prefix) + length($ul)) / $columns) + 1;
        }
    } else {
        $unfinished_rows_count = $unfinished_count;
    }
    if ($unfinished_rows_count > 1) {
        # Go $unfinished_rows_count lines backward.
        my $backward = $unfinished_rows_count - 1;
        print STDERR "\e[${backward}A\r";
    } elsif ($unfinished_rows_count > 0) {
        # Only go to the beginning of the line.
        print STDERR "\r";
    }

    my $newline = (chomp $line) ? "\n" : '';

    if ($color_start) {
        $line =~ s,<b>,\e[1m,o;
        $line =~ s,</b>,\e[0m,o;
    } else {
        $line =~ s,<b>,\e[32m,o;
        $line =~ s,</b>,\e[0m,o;
    }

    my $rewind = '';
    if ($line =~ /<r\/>/o) {
        $line   =~ s,<r/>,,go;
        $rewind = "\r";
    }

    if ($newline) {
        # Remove the previous unfinished context, if any.
        my $state = $self->{'unfinished_ctx'}->{$context};
        if ($state) {
            my $i = $state->{'line'};
            my $whole_line = splice @{$self->{'unfinished_lines'}}, $i, 1;
            delete $self->{'unfinished_ctx'}->{$context};

            $line = $state->{'rewind'} ? $line : $whole_line.$line;
        }

        print STDERR "$rewind$prefix$color_start$line$color_end\e[K\n";
    } else {
        my $state = $self->{'unfinished_ctx'}->{$context};
        if ($state) {
            my $i = $state->{'line'};
            if ($rewind) {
                $self->{'unfinished_lines'}->[$i] = $line;
            } else {
                $self->{'unfinished_lines'}->[$i] .= $line;
            }
        } else {
            push @{$self->{'unfinished_lines'}}, $line;
            $self->{'unfinished_ctx'}->{$context} = {
                'line'        => $unfinished_count,
                'color_start' => $color_start,
                'color_end'   => $color_end,
                'rewind'      => $rewind
            };
            $unfinished_count++;
        }
    }

    if ($unfinished_count > 0) {
        my @lines = ();
        foreach my $state (values %{$self->{'unfinished_ctx'}}) {
            my $i           = $state->{'line'};
            my $color_start = $state->{'color_start'};
            my $color_end   = $state->{'color_end'};
            my $rewind      = $state->{'rewind'};
            my $line        = $self->{'unfinished_lines'}->[$i];

            $lines[$state->{'line'}] =
                "$rewind$prefix$color_start$line$color_end";
        }
        my $lines = join("\e[K\n", @lines)."\e[K";
        print STDERR $lines;
    }
}

sub _syslog ($@) {
    my ($self, $context, @lines) = @_;

    return unless (scalar @lines > 0);

    foreach my $line (@lines) {
        $self->_syslog_line($context, $line);
    }
}

sub _syslog_line ($$) {
    my ($self, $context, $line) = @_;

    my $priority;
    if ($context eq 'ERROR') {
        $priority = LOG_ERR;
    } elsif ($context eq 'WARNING') {
        $priority = LOG_WARNING;
    } elsif ($context eq 'INFO') {
        $priority = LOG_NOTICE
    } else {
        $priority = LOG_DEBUG
    }

    # Remove formatting tags.
    $line =~ s/<\/?[br]>//go;

    syslog($priority, $line);
}

sub show_cursor ($) {
    my ($self, $show) = @_;

    if ($show) {
        print STDERR "\e[?25h";
    } else {
        print STDERR "\e[?25l";
    }
}

sub waiting ($) {
    my ($self, $waiting) = @_;

    if ($waiting) {
        # Check that we're not already waiting.
        return if ($self->{'waiting'});
        $self->{'waiting'} = 1;

        # Start spinning.
        $SIG{'ALRM'} = sub {
            if ($self->{'waiting'} && !$self->{'updating'}) {
                if ($self->{'spinning'}) {
                    my $prev_spin = $SPIN[$self->{'spin_frame'}];
                    print STDERR ("\b" x length($prev_spin)).$self->_next_spin;
                } else {
                    print STDERR $self->_next_spin;
                }
                $self->{'spinning'} = 1;
            }
        };
        setitimer(ITIMER_REAL, $SPIN_INTV, $SPIN_INTV);

        print STDERR $self->_next_spin;
        $self->{'spinning'} = 1;
    } else {
        return unless ($self->{'waiting'});
        $self->{'waiting'} = 0;

        # Stop spinning.
        setitimer(ITIMER_REAL, 0);

        if ($self->{'spinning'}) {
            # Remove the last frame of the animation.
            my $prev_spin = $SPIN[$self->{'spin_frame'}];
            print STDERR ("\b" x length($prev_spin))."\e[K";
            $self->{'spinning'} = 0;
        }
    }
}

sub is_waiting () {
    my ($self) = @_;

    return $self->{'waiting'};
}

sub _next_spin () {
    my ($self) = @_;

    my $n = $self->{'spin_frame'};
    $self->{'spin_frame'} = ($n + 1) % scalar(@SPIN);

    return $SPIN[$n];
}

sub app () { shift->{'app'}; }

1;
