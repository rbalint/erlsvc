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

package ErlSvc::Ctl::Mnesia;

use strict;
use warnings;
use utf8;

use File::Path qw(rmtree);
use ErlSvc::Ctl::Release;

sub new ($) {
    my ($class, $app) = @_;
    $class = ref($class) || $class;

    my $self = {
        'app' => $app
    };

    bless $self => $class;
}

sub directory (;%) {
    my ($self, %opts) = @_;

    # Get the target node and check he's running.
    my $target = $self->app->node;

    my $node;
    if (!$target->is_alive || $target->autostarted) {
        # The target node is down. We check if he's local to this
        # host to determine if we can start it.
        if (!$target->is_local) {
            $self->app->log->error(
                "Problem:\n",
                "  The node '$target' is not started and running this\n",
                "  action on a remote host isn't supported.\n"
            );

            return;
        }

        $target->stop;
        $self->_set_dir_or_release($target, %opts);
        $node = $target;
    } else {
        # The command will run on the target node but we use the controller
        # node as a gateway.
        my $controller = $self->app->controller;
        $controller->set_target($target) or return;

        $node = $controller;
    }

    my $script  = ErlSvc::Ctl::Erlang::Script->new($self->app);
    my $command = "{erlsvc_mnesia, directory, []}";

    $self->app->log->debug("MNESIA",
        "Query Mnesia directory on node '$node'\n");

    my %result = $script->eval($node, $command);

    if ($target->autostarted) {
        # The target was started by this function. We stop it because
        # the node has special arguments for Mnesia and we don't want
        # any potential side effects.
        $target->stop;
    }

    unless ($result{'status'} && $result{'status'} eq 'ok') {
        return;
    }

    $self->app->log->debug("MNESIA",
        "Mnesia directory: ".$result{'return'}."\n");

    return $result{'return'};
}

sub is_directory_used (;%) {
    my ($self, %opts) = @_;

    # Get the target node and check he's running.
    my $target = $self->app->node;

    my $node;
    if (!$target->is_alive || $target->autostarted) {
        # The target node is down. We check if he's local to this
        # host to determine if we can start it.
        if (!$target->is_local) {
            $self->app->log->error(
                "Problem:\n",
                "  The node '$target' is not started and running this\n",
                "  action on a remote host isn't supported.\n"
            );

            return;
        }

        $target->stop;
        $self->_set_dir_or_release($target, %opts);
        $node = $target;
    } else {
        # The command will run on the target node but we use the controller
        # node as a gateway.
        my $controller = $self->app->controller;
        $controller->set_target($target) or return;

        $node = $controller;
    }

    my $script  = ErlSvc::Ctl::Erlang::Script->new($self->app);
    my $command = "{erlsvc_mnesia, is_directory_used, []}";

    $self->app->log->debug("MNESIA", 'Check if Mnesia directory is used');

    my %result = $script->eval($node, $command);

    if ($target->autostarted) {
        # The target was started by this function. We stop it because
        # the node has special arguments for Mnesia and we don't want
        # any potential side effects.
        $target->stop;
    }

    unless ($result{'status'} && $result{'status'} eq 'ok') {
        return;
    }

    if ($result{'return'} eq 'true') {
        $self->app->log->debug("MNESIA", 'Mnesia directory is in use');
        return 1;
    } else {
        $self->app->log->debug("MNESIA", 'Mnesia directory is unused');
        return 0;
    }
}

sub cluster_nodes (;%) {
    my ($self, %opts) = @_;

    # Get the target node and check he's running.
    my $target = $self->app->node;

    my $node;
    if (!$target->is_alive || $target->autostarted) {
        # The target node is down. We check if he's local to this
        # host to determine if we can start it.
        if (!$target->is_local) {
            $self->app->log->error(
                "Problem:\n",
                "  The node '$target' is not started and running this\n",
                "  action on a remote host isn't supported.\n"
            );

            return;
        }

        $target->stop;
        $self->_set_dir_or_release($target, %opts);
        $node = $target;
    } else {
        # The command will run on the target node but we use the controller
        # node as a gateway.
        my $controller = $self->app->controller;
        $controller->set_target($target) or return;

        $node = $controller;
    }

    my $script  = ErlSvc::Ctl::Erlang::Script->new($self->app);
    my $command = "{erlsvc_mnesia, db_nodes, []}";

    $self->app->log->debug("MNESIA", "Query Mnesia DB nodes on node '$node'\n");

    my %result = $script->eval($node, $command);

    if ($target->autostarted) {
        # The target was started by this function. We stop it because
        # the node has special arguments for Mnesia and we don't want
        # any potential side effects.
        $target->stop;
    }

    unless ($result{'status'} && $result{'status'} eq 'ok') {
        return;
    }

    my %nodes = %{$result{'return'}};
    foreach my $node (keys %nodes) {
        $nodes{$node} = ($nodes{$node} eq 'up') ? 1 : 0;
    }

    my $debug_sub = sub {
        my @output = ("Cluster nodes:\n");
        foreach my $node (keys %nodes) {
            push @output, " - $node".
            ' ('.($nodes{$node} ? 'UP' : 'DOWN').")\n";
        }
        return @output;
    };
    $self->app->log->debug("MNESIA", $debug_sub);

    return %nodes;
}

sub create_schema (;%) {
    my ($self, %opts) = @_;

    # Get the target node and check he's running.
    my $target = $self->app->node;

    if (!$target->is_local) {
        $self->app->log->error(
            "Problem:\n",
            "  The node '$target' is remote.\n"
        );

        return;
    } elsif ($target->is_alive && !$target->autostarted) {
        $self->app->log->error(
            "Problem:\n",
            "  The node '$target' is already started.\n",
            "\n",
            "Solution(s):\n",
            "  1. Stop the node and call this command again.\n"
        );

        return;
    }

    $target->stop;
    $target->proc->run_as_user($self->app->user, $self->app->group);

    if ($opts{'directory'}) {
        $self->app->log->debug("MNESIA",
            'Create Mnesia schema (directory: \''.
            $opts{'directory'}."')\n");

        $target->set_erl_app_args('mnesia' => {
                'dir' => '"'.$opts{'directory'}.'"'
            });
    } elsif ($opts{'release'}) {
        $self->app->log->debug("MNESIA",
            'Create Mnesia schema (directory: '.
            'taken from release \''.$opts{'release'}."' sys.config)\n");

        $target->use_release($opts{'release'});
    }

    my $script = ErlSvc::Ctl::Erlang::Script->new($self->app);
    my %result;

    if ($opts{'cluster'}) {
        unless (ref($opts{'cluster'}) eq 'ARRAY') {
            $self->app->log->error(
                "Problem:\n",
                "  The cluster argument must be an array even if it\n",
                "  contains only one node.\n");
            return;
        }

        my @nodes = map { "'$_'" } @{$opts{'cluster'}};
        my $extra_db_nodes = join(', ', @nodes);
        $self->app->log->debug("MNESIA",
            "Join cluster using remote nodes $extra_db_nodes\n");

        my $command = "{erlsvc_mnesia, join_cluster, [[$extra_db_nodes]]}";
        %result = $script->eval($target, $command);
    } else {
        my $command = "{erlsvc_mnesia, create_schema, []}";
        %result = $script->eval($target, $command);
    }

    # The target was started by this function. We stop it because
    # the node has special arguments for Mnesia and we don't want
    # any potential side effects.
    $target->stop;
    $target->proc->run_as_user(undef, undef);

    unless ($result{'status'} && $result{'status'} eq 'ok') {
        return;
    }

    return 1;
}

sub remove_schema (;%) {
    my ($self, %opts) = @_;

    # Get the target node and check he's running.
    my $target = $self->app->node;

    if (!$target->is_local) {
        $self->app->log->error(
            "Problem:\n",
            "  The node '$target' is remote.\n"
        );

        return;
    } elsif ($target->is_alive && !$target->autostarted) {
        $self->app->log->error(
            "Problem:\n",
            "  The node '$target' is already started.\n",
            "\n",
            "Solution(s):\n",
            "  1. Stop the node and call this command again.\n"
        );

        return;
    }

    $target->stop;

    if ($opts{'directory'}) {
        $self->app->log->debug("MNESIA",
            'Leave Mnesia cluster (directory: \''.
            $opts{'directory'}."')\n");

        $target->set_erl_app_args('mnesia' => {
                'dir' => '"'.$opts{'directory'}.'"'
            });
    } elsif ($opts{'release'}) {
        $self->app->log->debug("MNESIA",
            'Leave Mnesia cluster (directory: '.
            'taken from release \''.$opts{'release'}."' sys.config)\n");

        $target->use_release($opts{'release'});
    }

    my $script  = ErlSvc::Ctl::Erlang::Script->new($self->app);
    my $command = "{erlsvc_mnesia, leave_cluster, []}";

    my %result = $script->eval($target, $command);

    # The target was started by this function. We stop it because
    # the node has special arguments for Mnesia and we don't want
    # any potential side effects.
    $target->stop;

    unless ($result{'status'} && $result{'status'} eq 'ok') {
        return;
    }

    my $dir = $opts{'directory'} ? $opts{'directory'} : $self->directory(%opts);
    $self->app->log->debug("MNESIA",
        "Remove files in directory '$dir'\n");

    my $errors;
    rmtree($dir, { 'keep_root' => 1, 'error' => \$errors });
    if (@$errors) {
        my $error_sub = sub {
            my @output = ();
            foreach my $error (@$errors) {
                my ($file, $msg) = %$error;
                if ($file eq '') {
                    push @output, "    General error: $msg\n";
                } else {
                    push @output, "    $file: $msg\n";
                }
            }
            return @output;
        };
        $self->app->log->error(
            "Problem:\n",
            "  Failed to remove Mnesia files in directory '$dir'\n",
            "  System reports:\n",
            $error_sub
        );

        return;
    }

    return 1;
}

sub check_consistency (;%) {
    my ($self, %opts) = @_;

    # Get the target node and check he's running.
    my $target = $self->app->node;

    if (!$target->is_alive || $target->autostarted) {
        # The target node is down: this is bad.
        $self->app->log->error(
            "Problem:\n",
            "  The node '$target' is not started.\n",
            "\n",
            "Solution(s):\n",
            "  1. Start this node.\n",
            "  2. Point to another node of the cluster.\n");
        return;
    }

    # The command will run on the controller.
    my $controller = $self->app->controller;
    $controller->unset_target;

    my $script  = ErlSvc::Ctl::Erlang::Script->new($self->app);
    my $verbose = $self->app->verbose('MNESIA') ? 'true' : 'false';
    my $command = "{erlsvc_mnesia, check_consistency, ".
      "['$target', [{verbose, $verbose}]]}";

    my %result = $script->eval($controller, $command);

    unless ($result{'status'} && $result{'status'} eq 'ok') {
        return;
    }

    return 1;
}

sub _init_script () {
    my ($self) = @_;

    my $script = ErlSvc::Ctl::Erlang::Script->new($self->app,
        'erlsvc_mnesia');
    $script->run_as_user($self->app->user, $self->app->group);

    my $node = $self->app->node;
    $script->set_node_identity($node);

    return $script;
}

sub _set_dir_or_release ($%) {
    my ($self, $node, %opts) = @_;

    if ($opts{'directory'}) {
        if (!$node->is_local) {
            $self->app->log->warning(
                'The specified directory is not taken into account when the '.
                'target node is remote.');
        } elsif ($node->is_alive) {
            $self->app->log->warning(
                'The specified directory is not taken into account when the '.
                'target node is alive.');
        } else {
            $node->set_erl_app_args('mnesia' => {
                    'dir' => '"'.$opts{'directory'}.'"'
                });
        }
    } elsif ($opts{'release'}) {
        if (!$node->is_local) {
            $self->app->log->warning(
                'The specified release is not taken into account when the '.
                'target node is remote.');
        } elsif ($node->is_alive) {
            $self->app->log->warning(
                'The specified release is not taken into account when the '.
                'target node is alive.');
        } else {
            $node->use_release($opts{'release'});
        }
    } elsif ($node->is_local && !$node->is_alive) {
        my $release = ErlSvc::Ctl::Release->new($self->app);
        my $current = $release->current;
        unless ($release->is_vanilla($current)) {
            $node->use_release($current);
        }
    }
}

sub app () { shift->{'app'}; }

1;
