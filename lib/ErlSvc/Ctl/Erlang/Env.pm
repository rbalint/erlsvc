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

package ErlSvc::Ctl::Erlang::Env;

use strict;
use warnings;
use utf8;

use File::Spec;
use ErlSvc::Ctl::Erlang::Script;
use ErlSvc::Ctl::Release;

sub new ($) {
    my ($class, $app) = @_;
    $class = ref($class) || $class;

    my $self = {
        'cmd' => {},
        'app' => $app
    };

    bless $self => $class;

    $self->_set_erl_cmd;

    return $self;
}

sub _set_erl_cmd (;$) {
    my ($self, $root_dir) = @_;

    # Set the path to erl(1).
    my $cmd = 'erl';
    $root_dir = $self->app->opts('erlang') unless ($root_dir);
    if ($root_dir && -d $root_dir) {
        my $erl_cmd = File::Spec->catfile($root_dir, 'bin', $cmd);
        if (-x $erl_cmd) {
            $self->app->log->debug('ERLENV',
                "$cmd(1) binary is '$erl_cmd' ".
                "(--erlang/-E was specified)\n");

            $self->{'cmd'}->{$cmd} = $erl_cmd;
        }
    }
    if (!$self->{'cmd'}->{$cmd} && $ENV{'ERL'}) {
        my $erl_cmd = $ENV{'ERL'};
        if (-x $erl_cmd) {
            $self->app->log->debug('ERLENV',
                "$cmd(1) binary is '$erl_cmd' ".
                "(taken from \$ERL environment variable)\n");

            $self->{'cmd'}->{$cmd} = $erl_cmd;
        }
    }
    if (!$self->{'cmd'}->{$cmd}) {
        $self->app->log->debug('ERLENV',
            "$cmd(1) binary is '$cmd' ".
            "(will be searched in \$PATH)\n");

        $self->{'cmd'}->{$cmd} = $cmd;
    }
}

sub finish_init() {
    my ($self) = @_;

    # Get the target node and check he's running.
    my $target     = $self->app->node;
    my $controller = $self->app->controller;

    if (!$target->is_alive) {
        # The target node is down. We check if he's local to this host
        # to determine if we can start it.
        if (!$target->is_local) {
            $self->app->log->error(
                "Problem:\n",
                "  The node '$target' is not started and can't be started\n",
                "  automatically because the host is remote.\n"
            );

            return;
        }
    } else {
        # The command will run on the target node but we use the controller
        # node as a gateway.
        $controller->set_target($target) or return;
    }

    my $script = ErlSvc::Ctl::Erlang::Script->new($self->app);
    my ($command, %result);

    # Set root dir.
    my $root_dir = $self->app->opts('erlang');
    unless ($root_dir) {
        $command = "{erlsvc_erlenv, root_dir, []}";
        %result  = $script->eval($controller, $command);
        unless ($result{'status'} && $result{'status'} eq 'ok') {
            return;
        }

        $root_dir = $result{'return'};
    }
    unless (-d $root_dir) {
        $self->app->log->error(
            "Problem:\n",
            "  The Erlang root directory '$root_dir' isn't usable.\n",
            "  System reports:\n",
            "    $!\n");
        return;
    }
    $self->{'root_dir'} = $root_dir;
    $self->app->log->debug('ERLENV',
        "Erlang root dir: '$root_dir'\n");

    # Set bin dir.
    $self->{'bin_dir'} = File::Spec->catfile($root_dir, 'bin');
    $self->app->log->debug('ERLENV',
        "Erlang bin dir: ".$self->{'bin_dir'}."\n");

    # The "releases" and "lib" directories are onl relevant for local
    # operation.
    if ($target->is_local) {
        # Set releases dir.
        my $releases_dir = $self->app->opts('releases_dir');
        unless ($releases_dir) {
            $command = "{erlsvc_erlenv, releases_dir, []}";
            %result  = $script->eval($controller, $command);
            unless ($result{'status'} && $result{'status'} eq 'ok') {
                return;
            }

            $releases_dir = $result{'return'};
        }
        unless (-d $releases_dir) {
            $self->app->log->error(
                "Problem:\n",
                "  The Erlang releases directory '$releases_dir' isn't ".
                "usable.\n",
                "  System reports:\n",
                "    $!\n");
            return;
        }
        $self->{'releases_dir'} = $releases_dir;
        $self->app->log->debug('ERLENV',
            "Erlang releases dir: $releases_dir\n");

        # Set lib dir.
        $command = "{erlsvc_erlenv, lib_dir, []}";
        %result  = $script->eval($controller, $command);
        unless ($result{'status'} && $result{'status'} eq 'ok') {
            return;
        }
        $self->{'lib_dir'} = $result{'return'};
        $self->app->log->debug('ERLENV',
            "Erlang lib dir: ".$self->{'lib_dir'}."\n");
    }

    # Set path for various commands.
    my @cmds = ('run_erl', 'start_erl', 'to_erl');
    foreach my $name (@cmds) {
        my $cmd = File::Spec->catfile($self->{'bin_dir'}, $name);
        $self->{'cmd'}->{$name} = $cmd;
        $self->app->log->debug('ERLENV',
            "Erlang $name(1) binary: $cmd\n");
    }

    # Set ERTS version and directory.
    $command = "{erlsvc_erlenv, erts_version, []}";
    %result  = $script->eval($controller, $command);
    unless ($result{'status'} && $result{'status'} eq 'ok') {
        return;
    }
    $self->{'erts_version'} = $result{'return'};
    $self->app->log->debug('ERLENV',
        "Erlang ERTS version: ".$self->{'erts_version'}."\n");
    $self->{'erts_dir'} = File::Spec->catfile($root_dir,
        'erts-'.$result{'return'});

    return 1;
}

sub change_root_dir ($) {
    my ($self, $root_dir) = @_;

    my $previous_root_dir = $self->root_dir;
    $self->app->log->debug('ERLENV',
        "Change Erlang root directory\n",
        "  from: $previous_root_dir\n",
        "  to:   $root_dir\n");

    # We first stop the controller and target nodes, because
    # they rely on the previous Erlang environment.
    $self->app->node->stop;
    $self->app->controller->stop;

    # Setup the environment again.
    $self->_set_erl_cmd($root_dir);
    $self->finish_init;
}

sub change_releases_dir ($) {
    my ($self, $releases_dir) = @_;

    $self->{'releases_dir'} = $releases_dir;
}

sub erts_version () {
    my ($self) = @_;

    return $self->{'erts_version'};
}

sub erts_dir () {
    my ($self) = @_;

    return $self->{'erts_dir'};
}

sub erl_cmd (;%) {
    my ($self, %opts) = @_;

    my $cmd = $opts{'cmd'} || 'erl';

    return $self->{'cmd'}->{$cmd};
}

sub root_dir () {
    my ($self) = @_;

    return $self->{'root_dir'};
}

sub bin_dir () {
    my ($self) = @_;

    return $self->{'bin_dir'};
}

sub lib_dir () {
    my ($self) = @_;

    return $self->{'lib_dir'};
}

sub releases_dir () {
    my ($self) = @_;

    return $self->{'releases_dir'};
}

sub default_releases_dir () {
    my ($self) = @_;

    return File::Spec->catfile($self->root_dir, 'releases');
}

sub release_dir ($) {
    my ($self, $release) = @_;

    return File::Spec->catfile($self->releases_dir, $release);
}

sub default_release_dir ($) {
    my ($self, $release) = @_;

    return File::Spec->catfile($self->default_releases_dir, $release);
}

sub release_boot_script ($) {
    my ($self, $release) = @_;

    my $boot_script = File::Spec->catfile($self->release_dir($release),
        'start.boot');
    my $boot_script_source = $boot_script;
    $boot_script_source =~ s/\.boot$/.rel/o;
    unless (-f $boot_script && -f $boot_script_source) {
        $boot_script = File::Spec->catfile($self->release_dir($release),
            'start_sasl.boot');
    }

    return $boot_script;
}

sub release_sysconfig ($) {
    my ($self, $release) = @_;

    return File::Spec->catfile($self->release_dir($release), 'sys.config');
}

sub release_relup ($) {
    my ($self, $release) = @_;

    return File::Spec->catfile($self->release_dir($release), 'relup');
}

sub start_erl_data () {
    my ($self) = @_;

    return File::Spec->catfile($self->releases_dir, 'start_erl.data');
}

sub parse_start_erl_data (;$) {
    my ($self, $start_erl_data) = @_;

    $start_erl_data = $self->start_erl_data unless ($start_erl_data);

    open(my $fh, '<', $start_erl_data) or return;
    my $first_line = <$fh>;
    close($fh);

    chomp($first_line);
    my @start_erl_data = split(/ /o, $first_line);

    my $debug_sub = sub {
        map { " $_\n"; } @start_erl_data;
    };
    $self->app->log->debug("ERLENV",
        "Erlang start_erl_data contains the following fields:\n",
        $debug_sub);

    return @start_erl_data;
}

sub RELEASES () {
    my ($self) = @_;

    return File::Spec->catfile($self->releases_dir, 'RELEASES');
}

sub app () { shift->{'app'}; }

1;
