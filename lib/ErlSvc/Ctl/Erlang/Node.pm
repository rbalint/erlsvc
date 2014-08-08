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

package ErlSvc::Ctl::Erlang::Node;

use strict;
use warnings;
use utf8;

use overload ('""' => 'stringify');

use Cwd qw(abs_path);
use POSIX qw(strftime);
use Sys::Hostname;
use ErlSvc::Ctl::Erlang::Script;
use ErlSvc::Ctl::Proc;

sub local_hostname () {
    my $hostname = hostname;
    $hostname =~ /^([^.]+)/o;

    return $1;
}

sub new ($$;$) {
    my ($class, $app, $node, $host) = @_;
    $class = ref($class) || $class;

    if ($node =~ /^([^@]+)@(.*)/o) {
        # The node name given contains the host part. Parse it.
        $node = $1;
        $host = $2;
    }

    # No host was specified. Therefore, it defaults to the local
    # hostname.
    $host = local_hostname() unless $host;

    # We determine if the node is local by looking at the host part.
    my $is_local = 0;
    if ($host) {
        $is_local = 1 if ($host eq local_hostname());
    } else {
        $host = local_hostname();
        $is_local = 1;
    }

    my $self = {
        'node'         => $node,
        'host'         => $host,
        'is_local'     => $is_local,
        'controller'   => 0,
        'erl_app_args' => {},
        'erl_cmd_args' => [],
        'proc'         => ErlSvc::Ctl::Proc->new($app),
        'app'          => $app
    };

    bless $self => $class;
}

sub new_from_app_opts ($) {
    my ($class, $app) = @_;

    my $node     = $app->opts('node');
    my $host     = $app->opts('host');
    my $cookie   = $app->opts('cookie');
    my $rels_dir = $app->opts('releases_dir');

    my $self = $class->new($app, $node, $host);
    $self->set_cookie($cookie);
    $self->set_releases_dir($rels_dir);

    return $self;
}

sub node_name () {
    my ($self) = @_;

    return $self->{'node'};
}

sub full_node_name () {
    my ($self) = @_;

    return $self->{'node'}.'@'.$self->{'host'};
}

sub cookie () {
    my ($self) = @_;

    return $self->{'cookie'};
}

sub set_cookie ($) {
    my ($self, $cookie) = @_;

    if ($cookie) {
        $self->{'cookie'} = $cookie;
    } else {
        delete $self->{'cookie'};
    }
}

sub releases_dir () {
    my ($self) = @_;

    return $self->{'releases_dir'};
}

sub set_releases_dir ($) {
    my ($self, $releases_dir) = @_;

    if ($releases_dir) {
        $self->{'releases_dir'} = $releases_dir;
    } else {
        delete $self->{'releases_dir'};
    }
}

sub is_local () {
    my ($self) = @_;

    return $self->{'is_local'};
}

sub is_controller () {
    my ($self) = @_;

    return $self->{'controller'};
}

sub flag_as_controller () {
    my ($self) = @_;

    $self->{'controller'} = 1;
    $self->{'target'}     = 'self';
}

sub use_release ($) {
    my ($self, $release) = @_;

    $self->{'release'} = $release;
}

sub add_erl_cmd_args (@) {
    my $self = shift;

    push @{$self->{'erl_cmd_args'}}, @_;
}

sub set_erl_app_args (%) {
    my ($self, %all_args) = @_;

    while (my ($app, $new_args) = each %all_args) {
        if (exists $self->{'erl_app_args'}->{$app}) {
            my %args = (
                %{$self->{'erl_app_args'}->{$app}},
                %$new_args
            );
            $self->{'erl_app_args'}->{$app} = \%args;
        } else {
            $self->{'erl_app_args'}->{$app} = $new_args;
        }
    }
}

sub proc () {
    my ($self) = @_;

    return $self->{'proc'};
}

sub autostarted () {
    my ($self) = @_;

    return ($self->proc->pid) ? 1 : 0;
}

sub start () {
    my ($self) = @_;

    # If the node is already started, return immediately.
    return 1 if ($self->proc->pid);

    my $path = $self->app->opts('mods_dir');

    $self->app->log->debug("ERLNODE",
        "<b>Prepare launch of Erlang node '$self'".
        ($self->is_controller ? ' (controller node)' : '')."</b>\n");

    my $erl_env = $self->app->erl_env;
    my $erl_cmd = $erl_env->erl_cmd;

    my @cmdline = (
        $erl_cmd, '+Bd', '-noshell', '-hidden',
        '-connect_all', 'false',
        '-sname', $self->node_name
    );

    if ($self->cookie) {
        push @cmdline, ('-setcookie', $self->cookie);
    }

    # Check if we must use a release's sys.config file.
    my $release = $self->{'release'};
    if ($release) {
        $self->app->log->debug("ERLNODE",
            "Use sys.config from release '$release'\n");

        my $sysconfig = $erl_env->release_sysconfig($release);
        push @cmdline, ('-config', $sysconfig);
    }

    # Set the modules path if specified.
    if (-d $path) {
        $path = abs_path($path);
        $self->app->log->debug("ERLNODE",
            "Use '$path' as modules directory (ie. -pa)\n");

        push @cmdline, ('-pa', $path);
    }

    # Setup SASL.
    $self->set_erl_app_args('sasl' => {
            'sasl_error_logger' => 'false'
        });
    if ($self->releases_dir) {
        if (!$self->{'erl_app_args'}->{'sasl'}->{'releases_dir'}) {
            $self->set_erl_app_args('sasl' => {
                    'releases_dir' => '"'.$self->releases_dir.'"'
                });
        }
    }

    # Push Erlang application arguments.
    while (my ($app, $args) = each %{$self->{'erl_app_args'}}) {
        while (my ($arg, $value) = each %$args) {
            push @cmdline, ("-$app", $arg, $value);
        }
    }

    # Push additionnal arguments specified by caller.
    push @cmdline, @{$self->{'erl_cmd_args'}};

    # Format script options.
    push @cmdline, ('-eval', "erlsvc:run(), halt().");

    # Set the erlsvc version in the environment.
    $self->proc->add_env('ERLSVC' => $ErlSvc::Ctl::VERSION);

    # Log final command line.
    my $debug_sub = sub {
        my @output = ("Final Erlang node command line:\n");
        my $line = '';
        foreach my $arg (@cmdline) {
            if (length($line) + length($arg) > 65) {
                push @output, " $line \\\n";
                $line = $arg;
            } elsif (!$line) {
                $line = $arg;
            } else {
                $line .= " $arg";
            }
        }
        push @output, " $line\n";
        return @output;
    };

    $self->app->log->debug("ERLNODE", $debug_sub);

    # Fork the process.
    $self->proc->start(@cmdline) or return;

    $self->_read_pre_command_output();

    return 1;
}

sub _read_pre_command_output () {
    my ($self) = @_;

    my $proc     = $self->proc;
    my $child_fh = $proc->fh;

    my $stop = 0;

    while (<$child_fh>) {
        chomp;
        my $line = $_;

        # Interpret Erlang output.
        if ($line =~ /^CTL READY$/o) {
            $self->app->log->debug("ERLNODE",
                "Erlang node '$self' ready!\n");
            $stop = 1;
        } else {
            $proc->interpret_generic_output($line);
        }

        last if ($stop);
    }
}

sub stop () {
    my ($self) = @_;

    # If the node is not started, return immediately.
    return unless ($self->autostarted);

    my $command = 'stop';

    $self->app->log->debug("ERLNODE",
        "<b>Stop Erlang node '$self'".
        ($self->is_controller ? ' (controller node)' : '')."</b>\n");

    my $script = ErlSvc::Ctl::Erlang::Script->new($self->app);
    $script->eval($self, $command, 'ignore_eof' => 1);

    $self->proc->wait_for_child;
}

sub is_alive () {
    my ($self) = @_;

    # To know if a node is alive, we make a ping from the controller
    # node.
    my $controller = $self->app->controller;

    $self->app->log->debug("ERLNODE",
        "<b>Ping Erlang node '$self' from '$controller'</b>\n");

    my $command = "{net_adm, ping, ['$self']}";
    my $script  = ErlSvc::Ctl::Erlang::Script->new($self->app);
    my %result  = $script->eval($controller, $command);

    if ($result{'status'} && $result{'status'} eq 'ok' &&
      $result{'return'} eq 'pong') {
        $self->app->log->debug("ERLNODE",
            "Erlang node '$self' is ALIVE\n");
        return 1;
    } else {
        $self->app->log->debug("ERLNODE",
            "Erlang node '$self' is DOWN\n");
        return 0;
    }
}

sub set_target ($) {
    my ($self, $target) = @_;

    # This is only available on a controller node.
    return unless ($self->is_controller);

    my $target_name = ref($target) ? $target->full_node_name : $target;
    return 1 if ($target_name eq $self->{'target'});

    # Start the node.
    $self->start or return;

    $self->app->log->debug("ERLNODE",
        "<b>Set Erlang target node to '$target'</b>\n");

    my $command = "{target_node, '$target'}";
    my $script  = ErlSvc::Ctl::Erlang::Script->new($self->app);
    my %result  = $script->eval($self, $command);

    if ($result{'status'} && $result{'status'} eq 'ok') {
        $self->{'target'} = $target_name;
        return 1;
    } else {
        $self->app->log->error(
            "Problem:\n",
            "  Failed to set target node to '$target'\n");
        $self->stop;
        return;
    }
}

sub unset_target () {
    my ($self) = @_;

    $self->set_target('self');
}

sub app ()       { shift->{'app'}; }
sub stringify () { shift->full_node_name; }

1;
