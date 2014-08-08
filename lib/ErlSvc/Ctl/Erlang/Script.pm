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

package ErlSvc::Ctl::Erlang::Script;

use strict;
use warnings;
use utf8;

use YAML::Tiny;
use Data::Dumper;
$Data::Dumper::Indent = 1;

sub new ($) {
    my ($class, $app) = @_;

    my $self = {
        'app' => $app
    };

    bless $self => $class;
}

sub eval ($$;%) {
    my ($self, $node, $command, %options) = @_;

    # Start the node.
    unless ($node->start) {
        $self->app->log->error(
            "Problem:\n",
            "  Failed to start node for command execution\n");
        return;
    }

    $self->app->log->debug("ERLSCRIPT",
        "Execute command '$command' on node '$node'\n");

    # Get the filehandle to communicate with the node.
    my $proc = $node->proc;

    # First step: send the command.
    $self->_send_command($proc, $command, %options);

    # Second step: read script output.
    my $result = $self->_read_child_output($proc, %options);

    # Cleanup known result keys.
    if ($result->{'return'}) {
        chomp $result->{'return'};
    }
    if ($result->{'reason'}) {
        chomp $result->{'reason'};
    }
    if ($result->{'stacktrace'}) {
        for (my $i = 0; $i < scalar @{$result->{'stacktrace'}}; ++$i) {
            chomp $result->{'stacktrace'}->[$i];
        }
    }

    if (keys %$result) {
        $self->app->log->debug("ERLSCRIPT", (
                "Result:\n",
                map { " $_\n" } split("\n", Dumper($result))
            ));
    } else {
        $self->app->log->debug("ERLSCRIPT", "Result: none\n");
    }

    if ($result->{'status'} && $result->{'status'} eq 'exception') {
        # The command raised an exception.
        my @stacktrace = ();
        if ($result->{'stacktrace'}) {
            @stacktrace = (
                "\n",
                "  The stacktrace is:\n",
                map { "    $_\n"; } @{$result->{'stacktrace'}}
            );
        }

        my @reason = map { "    $_\n" } split("\n", $result->{'reason'});

        $self->app->log->error(
            "Problem:\n",
            "  The following command raised an exception:\n",
            "    $command\n",
            "\n",
            "  The reason is:\n",
            @reason,
            @stacktrace
        );

        return;
    }

    return %$result;
}

sub _send_command ($$;%) {
    my ($self, $proc, $command) = @_;

    my $child_fh = $proc->fh;
    print $child_fh $command.".\n";
}

sub _read_child_output ($;%) {
    my ($self, $proc, %options) = @_;

    my $child_fh = $proc->fh;

    my @result_lines = ();
    my $stop = 0;

    while (<$child_fh>) {
        chomp;
        my $line = $_;

        # Interpret Erlang output.
        if ($line =~ /^CTL RESULT BEGIN$/o) {
            # Ok, the result!
            # The result start with the line "CTL RESULT BEGIN" and ends
            # with the line "CTL RESULT END". The format used is YAML.
            while (<$child_fh>) {
                chomp;
                $line = $_;
                if ($line =~ /^CTL RESULT END$/o) {
                    # We reached the end of the result text. 
                    $stop = 1;
                    last;
                } else {
                    push @result_lines, $line;
                }
            }
        } else {
            $proc->interpret_generic_output($line);
        }

        last if ($stop);
    }

    if ($stop == 0) {
        unless ($options{'ignore_eof'}) {
            $self->app->log->debug("ERLSCRIPT",
                "Child file handle closed: the process exited prematurely!\n");
            $proc->wait_for_child();
        }
        return;
    }

    my $yaml;

    {
        # With a few parsing errors, YAML::Tiny dies instead of returning
        # "undef". Here, we want to catch this signal.
        local $@;

        $yaml = eval {
            local $SIG{'__DIE__'};
            YAML::Tiny->read_string(join("\n", @result_lines)."\n")
        };

        unless ($yaml) {
            my $yaml_errstr = $@ || YAML::Tiny->errstr;
            $self->app->log->error(
                "Problem:\n",
                "  Failed to parse result from script:\n",
                "  The YAML parser reports:\n",
                "    $yaml_errstr\n",
                "\n",
                "  The command output was:\n",
                map { "    $_\n"; } @result_lines);
            return;
        }
    }

    return $yaml->[0];
}

sub app () { shift->{'app'}; }

1;
