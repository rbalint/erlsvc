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

package ErlSvc::Ctl::Release;

use strict;
use warnings;
use utf8;

use File::Copy;
use File::stat;
use File::Path qw(rmtree);
require ErlSvc::Ctl::Target;

sub new ($) {
    my ($class, $app) = @_;
    $class = ref($class) || $class;

    my $self = {
        'app' => $app
    };

    bless $self => $class;
}

sub list () {
    my ($self) = @_;

    # Get the target node and check he's running.
    my $target     = $self->app->node;
    my $controller = $self->app->controller;

    if (!$target->is_alive) {
        # The target node is down. We check if he's local to this
        # host to determine if we can execute this action with the
        # controller.
        if (!$target->is_local) {
            $self->app->log->error(
                "Problem:\n",
                "  The node '$target' is not started and running this\n",
                "  action on a remote host isn't supported.\n"
            );

            return;
        }
    } else {
        # The command will run on the target node but we use the controller
        # node as a gateway.
        $controller->set_target($target) or return;
    }

    my $script = ErlSvc::Ctl::Erlang::Script->new($self->app);

    $self->app->log->debug("REL", "<b>Query releases list</b>\n");
    my $command = '{erlsvc_release, list, []}';

    my %result = $script->eval($controller, $command);
    unless ($result{'status'} && $result{'status'} eq 'ok') {
        return;
    }

    return %{$result{'return'}};
}

sub current () {
    my ($self) = @_;

    my %rels = $self->list;
    return unless (%rels);

    $self->app->log->debug("REL",
        "<b>Determine current release name</b>\n");

    my $permanent;
    foreach my $rel (keys %rels) {
        if ($rels{$rel}->{'state'} eq 'current') {
            $self->app->log->debug("REL",
                "Current release name: $rel (current)\n");
            return $rel;
        } elsif ($rels{$rel}->{'state'} eq 'permanent') {
            $permanent = $rel;
        }
    }

    if ($permanent) {
        $self->app->log->debug("REL",
            "Current release name: $permanent (permanent)\n");
        return $permanent;
    }

    return;
}

sub default () {
    my ($self) = @_;

    my %rels = $self->list;
    return unless (%rels);

    $self->app->log->debug("REL",
        "<b>Determine default release name</b>\n");

    foreach my $rel (keys %rels) {
        if ($rels{$rel}->{'state'} eq 'permanent') {
            $self->app->log->debug("REL",
                "Default release name: $rel\n");
            return $rel;
        }
    }

    return;
}

sub exists ($) {
    my ($self, $release) = @_;

    my %rels = $self->list;
    return unless (%rels);

    $self->app->log->debug("REL",
        "<b>Check if release '$release' exists</b>\n");

    unless (exists $rels{$release}) {
        # The release doesn't exist at all.
        $self->app->log->debug("REL",
            "Release '$release' doesn't exist: ".
            "no entry in the RELEASES file\n");
        return;
    }

    # Ok, the release exists. But we must ensure that it contains a
    # sys.config file.
    my $erl_env = $self->app->erl_env;
    unless ($erl_env->release_sysconfig($release)) {
        $self->app->log->debug("REL",
            "Release '$release' doesn't exist: no sys.config file\n");
        return;
    }

    $self->app->log->debug("REL", "Release '$release' exists\n");
    return 1;
}

sub state ($) {
    my ($self, $release) = @_;

    my %rels = $self->list;
    return unless (%rels);

    $self->app->log->debug("REL",
        "<b>Determine release '$release' state</b>\n");

    unless (exists $rels{$release}) {
        $self->app->log->debug("REL",
            "Release '$release' doesn't exist\n");
    }

    my $state = $rels{$release}->{'state'};

    $self->app->log->debug("REL",
        "Release '$release' state: $state\n");

    return $state;
}

sub upgradable ($;%) {
    my ($self, $release, %opts) = @_;

    # Get the target node and check he's running.
    my $target     = $self->app->node;
    my $controller = $self->app->controller;

    if (!$target->is_alive) {
        # The target node is down. We check if he's local to this
        # host to determine if we can execute this action with the
        # controller.
        if (!$target->is_local) {
            $self->app->log->error(
                "Problem:\n",
                "  The node '$target' is not started and running this\n",
                "  action on a remote host isn't supported.\n"
            );

            return;
        }
    } else {
        # The command will run on the target node but we use the controller
        # node as a gateway.
        $controller->set_target($target) or return;
    }

    $self->app->log->debug("REL",
        "<b>Verify that the current release can be upgraded to ".
        "release '$release'</b>\n");

    my $erl_env = $self->app->erl_env;

    my $relup = $opts{'relup'} || $erl_env->release_relup($release);
    if (-f $relup) {
        # Determine the release to upgrade from.
        my $service = ErlSvc::Ctl::Service->new($self->app);
        my $from;
        if ($opts{'from'}) {
            # The release was specified by the caller.
            $from = $opts{'from'};
        } elsif ($service->is_running) {
            # Use the running service's release.
            $from = $self->current;
        } else {
            # Use the release from the "start_erl.data" file.
            my $start_erl_data = $erl_env->start_erl_data;
            my ($erts, $default_release) = $erl_env->parse_start_erl_data(
                $start_erl_data);

            unless ($default_release) {
                $self->app->log->error(
                    "Problem:\n",
                    "  Failed to determined the current release.\n",
                    "  The file '$start_erl_data' couln't be parsed.\n");
                return;
            }

            $from = $default_release;
        }

        my $script  = ErlSvc::Ctl::Erlang::Script->new($self->app);
        my $command = "{erlsvc_release, upgradable, [\"$relup\", \"$from\"]}";

        my %result = $script->eval($controller, $command);
        if ($result{'status'} && $result{'status'} eq 'ok') {
            if ($result{'return'} eq 'true') {
                $self->app->log->debug('REL',
                    "Release '$from' can be upgraded to '$release'\n");
                return 1;
            } else {
                $self->app->log->debug('REL',
                    "Release '$from' CANNOT be upgraded to '$release'\n");
                return;
            }
        }
    } else {
        $self->app->log->debug('REL',
            "Release '$release' doesn't support live upgrade\n",
            "Relup script '$relup' unreadable:\n",
            "  $!\n");
    }

    return;
}

sub is_vanilla (;$) {
    my ($self, $release) = @_;

    $release = $self->current unless ($release);

    return $release =~ /^R?\d+[A|B]?\d*$/o;
}

sub upgrade ($;%) {
    my ($self, $release, %opts) = @_;

    my $target     = $self->app->node;
    my $controller = $self->app->controller;

    $self->app->log->debug('REL',
        "<b>Upgrade node '$target' to release '$release'</b>\n");

    my $erl_env  = $self->app->erl_env;
    my $rels_dir = $erl_env->releases_dir;
    unless (-f $erl_env->RELEASES) {
        $self->app->log->error(
            "Problem:\n",
            "  The releases directory must be bootstrapped first.\n",
            "  The releases directory is set to:\n",
            "    $rels_dir\n",
            "\n",
            "Solution(s):\n",
            "  1. Bootstrap the directory using the \"release syncvanilla\"\n",
            "     command.\n"
        );

        return;
    }

    # Check that the current release isn't already the target release.
    my $current_release = $self->current;
    if ($release eq $current_release) {
        $self->app->log->debug('REL',
            "Release '$release' is already installed\n");
        return 2;
    }

    # Check that node is local. Remote operation isn't supported because
    # it may need a node restart.
    if (!$target->is_local)  {
        $self->app->log->error(
            "Problem:\n",
            "  The node '$target' is remote.\n"
        );

        return;
    }

    my $rel_dir     = $erl_env->release_dir($release);
    my $boot_script = $erl_env->release_boot_script($release);
    my $sys_config  = $erl_env->release_sysconfig($release);

    # The caller is responsible for installing release's files in the
    # releases directory.
    my $boot_script_source = $erl_env->release_boot_script($release);
    unless (-d $rel_dir && -f $boot_script && -f $sys_config) {
        $self->app->log->error(
            "Problem:\n",
            "  The release '$release' isn't available on node '$target'\n",
            "\n",
            "Solution(s):\n",
            "  1. Check that the release directory exists. It should\n",
            "     be '$rel_dir'.\n",
            "  2. Check that this directory contains a 'start.boot' file\n",
            "     and a 'sys.config' file.\n"
        );

        return 0;
    }

    my %ret;

    my $ignore_is_running_test = $opts{'ignore_is_running_test'};
    my $service = ErlSvc::Ctl::Service->new($self->app);
    my $is_running = $service->is_running unless ($ignore_is_running_test);

    my $relup = $erl_env->release_relup($release);
    if (($ignore_is_running_test || $is_running) &&
      $self->upgradable($release, 'relup' => $relup)) {
        # The new release supports live upgrade. The steps are as follows:
        #   1. mark the release as unpacked (if it's not already done)
        #   2. install the release
        #   3. make it permanent

        my $node;
        if (!$target->is_alive) {
            # The target node is down. It'll be started for the upgrade,
            # but the service won't run on it.
            $node = $target;
            $target->proc->run_as_user($self->app->user, $self->app->group);
        } else {
            # The command will run on the target node but we use the controller
            # node as a gateway.
            $controller->set_target($target) or return;

            $node = $controller;
        }

        # When the node is a target system, we need to synchronize
        # the lib directory: the source environment may contain new
        # applications.
        my $target_system = ErlSvc::Ctl::Target->new($self->app);
        if ($target_system->is_target_system) {
            $target_system->sync_lib_dir or return;
        }

        my $script = ErlSvc::Ctl::Erlang::Script->new($self->app);

        # The node may have to update files which don't belong to the
        # user/group running the script.
        my %permissions = $self->_grant_permissions;

        $self->app->log->debug('REL',
            "Mark release '$release' as unpacked\n");

        my $boot_script_source = $boot_script;
        $boot_script_source =~ s/\.boot$/.rel/o;

        my $command = "{erlsvc_release, set_unpacked, [\"$boot_script_source\"]}";
        my %result = $script->eval($node, $command);
        unless ($result{'status'} && $result{'status'} eq 'ok') {
            $self->_restore_permissions(%permissions);
            return;
        }

        if ($release ne $result{'return'}) {
            $self->app->log->error(
                "Problem:\n",
                "  The given release ($release) and the unpacked\n",
                "  release (".$result{'return'}.") doesn't match.\n");
            $self->_restore_permissions(%permissions);
            return;
        }

        $self->app->log->debug('REL', "Proceed with upgrade to '$release'\n");

        $command = "{erlsvc_release, upgrade, [\"$release\"]}";
        %result = $script->eval($node, $command);

        # After upgrade, we must restore files ownership.
        $self->_restore_permissions(%permissions);

        unless ($result{'status'} && $result{'status'} eq 'ok') {
            return;
        }

        my $old_release = $opts{'original_release'} || $current_release;
        unless ($self->is_vanilla($old_release)) {
            my $ret = $self->remove($old_release);
            unless ($ret) {
                $self->app->log->warning(
                    "Failed to mark release '$old_release' ".
                    "as removed\n");
            }
        }
    } else {
        # The new release doesn't support live upgrade. We must do the
        # following steps:
        #   1. query the start options
        #   2. stop the service
        #   3. reset the release (back to Erlang vanilla release)
        #   4. do "hot" upgrade
        #   5. start the service

        my %start_opts = ();
        if (exists $opts{'extra_flags'}) {
            $start_opts{'extra_flags'} = $opts{'extra_flags'};
        }
        $service->add_start_opts_of_running_node(\%start_opts);

        my $ret;
        if ($is_running) {
            $ret = $service->stop();
            return unless ($ret);
        }

        $ret = $self->reset();
        return unless ($ret);
        if ($opts{'ignore_is_running_test'} && $ret == 2) {
            $self->app->log->error(
                "Problem:\n",
                "  The given release ($release) doesn't support upgrade from\n",
                "  Erlang vanilla release '$current_release'.\n");
            return;
        }

        # The new release may include a new Erlang version. The
        # reset above restored the old vanilla release. We must call
        # synchronize with the new vanilla before proceeding with
        # upgrade.
        $ret = $self->sync_vanilla_releases();

        $ret = $self->upgrade($release, %opts,
            'ignore_is_running_test' => 1,
            'original_release'       => $current_release
        );

        # The target node is started during upgrade but not the service.
        # We stop the node or the service will refuse to start.
        $controller->unset_target;
        $target->stop;
        return unless ($ret);

        if ($is_running) {
            $ret = $service->start(%start_opts);
            return unless ($ret);
        }
    }

    return 1;
}

sub reset () {
    my ($self) = @_;

    # Check that the current release isn't a vanilla release.
    my $current_release = $self->current;
    if ($self->is_vanilla($current_release)) {
        $self->app->log->debug('REL',
            "The current release is already a vanilla release\n",
            "No need to reset\n");
        return 2;
    }

    # Get the target node and check he's running.
    my $target     = $self->app->node;
    my $controller = $self->app->controller;

    if (!$target->is_alive) {
        # The target node is down. We check if he's local to this
        # host to determine if we can execute this action with the
        # controller.
        if (!$target->is_local) {
            $self->app->log->error(
                "Problem:\n",
                "  The node '$target' is not started and running this\n",
                "  action on a remote host isn't supported.\n"
            );

            return;
        }
    } else {
        # The command will run on the target node but we use the controller
        # node as a gateway.
        $controller->set_target($target) or return;
    }

    $self->app->log->debug("REL",
        "<b>Reset release to Erlang vanilla release</b>\n");

    # The node may have to update files which don't belong to the
    # user/group running the script.
    my %permissions = $self->_grant_permissions;

    my $script  = ErlSvc::Ctl::Erlang::Script->new($self->app);
    my $command = "{erlsvc_release, reset, []}";
    my %result  = $script->eval($controller, $command);

    # After upgrade, we must restore files ownership.
    $self->_restore_permissions(%permissions);

    unless ($result{'status'} && $result{'status'} eq 'ok') {
        return;
    }

    return 1;
}

sub remove ($) {
    my ($self, $release) = @_;

    # It's forbidden to remove an Erlang release (eg. R13B04).
    if ($self->is_vanilla($release)) {
        $self->app->log->error(
            "Problem:\n",
            "  Removing an Erlang vanilla release is not allowed.\n");
        return;
    }

    # Get the target node and check he's running.
    my $target     = $self->app->node;
    my $controller = $self->app->controller;

    if (!$target->is_alive) {
        # The target node is down. We check if he's local to this
        # host to determine if we can execute this action with the
        # controller.
        if (!$target->is_local) {
            $self->app->log->error(
                "Problem:\n",
                "  The node '$target' is not started and running this\n",
                "  action on a remote host isn't supported.\n"
            );

            return;
        }
    } else {
        # The command will run on the target node but we use the controller
        # node as a gateway.
        $controller->set_target($target) or return;
    }

    $self->app->log->debug("REL",
        "<b>Mark release '$release' as removed</b>\n");

    # The node may have to update files which don't belong to the
    # user/group running the script.
    my %permissions = $self->_grant_permissions;

    my $script  = ErlSvc::Ctl::Erlang::Script->new($self->app);
    my $command = "{erlsvc_release, set_removed, [\"$release\"]}";
    my %result  = $script->eval($controller, $command);

    # After upgrade, we must restore files ownership.
    $self->_restore_permissions(%permissions);

    unless ($result{'status'} && $result{'status'} eq 'ok') {
        return;
    }

    return 1;
}

sub sync_vanilla_releases (;%) {
    my ($self, %opts) = @_;

    $self->app->log->debug('REL',
        "<b>Synchronize Erlang vanilla releases</b>\n");

    my $erl_env          = $self->app->erl_env;
    my $rels_dir         = $erl_env->releases_dir;
    my $default_rels_dir = $erl_env->default_releases_dir;
    $self->app->log->debug('REL',
        "Source:      $default_rels_dir\n",
        "Destination: $rels_dir\n");
    return 1 if ($rels_dir eq $default_rels_dir);

    if ($opts{'from_scratch'}) {
        $self->_clear_releases_directory($rels_dir);
    }

    # To update the release_handler's state files, we must use an
    # Erlang node.
    my $target     = $self->app->node;
    my $controller = $self->app->controller;

    if (!$target->is_alive) {
        # The target node is down. We check if he's local to this
        # host to determine if we can execute this action with the
        # controller.
        if (!$target->is_local) {
            $self->app->log->error(
                "Problem:\n",
                "  The node '$target' is not started and running this\n",
                "  action on a remote host isn't supported.\n"
            );

            return;
        }
    } else {
        # The command will run on the target node but we use the controller
        # node as a gateway.
        $controller->set_target($target) or return;
    }

    my $script = ErlSvc::Ctl::Erlang::Script->new($self->app);

    # We first determine the list of releases.
    my @default_rels = $self->_list_vanilla_releases($script, $controller,
        $default_rels_dir, 'in default directory');
    return unless (scalar @default_rels);

    if (-e $erl_env->RELEASES) {
        # The releases directory was previously populated. Our job is to
        # check if a new Erlang vanilla release is available and add it.
        $self->app->log->debug('REL',
            "Releases directory already populated: update\n");

        # List "old" vanilla releases.
        my @effective_rels = $self->_list_vanilla_releases($script, $controller,
            $rels_dir, 'in effective directory');
        my %old_rels = ();
        my %new_rels = ();
        foreach my $rel (@default_rels, @effective_rels) {
            my $in_default   = grep { $rel eq $_; } @default_rels;
            my $in_effective = grep { $rel eq $_; } @effective_rels;
            if ($in_default && $in_effective) {
                next;
            } elsif ($in_default) {
                $new_rels{$rel} = 1;
            } else {
                $old_rels{$rel} = 1;
            }
        }
        my @old_rels = sort(keys %old_rels);
        my @new_rels = sort(keys %new_rels);

        if (scalar @old_rels == 0 && scalar @new_rels == 0) {
            $self->app->log->debug('REL',
                "Effective directory already up-to-date\n");
            return 2;
        }

        # Copy releases.
        foreach my $rel (@new_rels) {
            my $ret = $self->_copy_vanilla_release($default_rels_dir, $rels_dir,
                $rel);
            return unless ($ret);
        }

        # We must check if one of the old releases is currently
        # permanent. This is used to set properly the state of the
        # newest release.
        my $permanent = 'none';
        foreach my $rel (@old_rels) {
            if ($self->state($rel) eq 'permanent') {
                $permanent = '"'.$new_rels[$#new_rels].'"';
                last;
            }
        }

        $self->app->log->debug('REL',
            "Set releases as unpacked:");
        foreach my $rel (@new_rels) {
            $self->app->log->debug('REL', " $rel");
            my $file = $erl_env->release_boot_script($rel);
            $file =~ s/\.boot$/.rel/o;

            my $command = "{erlsvc_release, set_unpacked, [\"$file\"]}";
            my %result  = $script->eval($controller, $command);

            unless ($result{'status'} && $result{'status'} eq 'ok') {
                $self->app->log->debug('REL', " failed.\n");
                return;
            }
        }
        $self->app->log->debug('REL', ".\n");

        my $command = '{erlsvc_release, sync_vanilla, [';
        $command .= '"'.$erl_env->releases_dir.'", ';
        $command .= '['.join(', ', map { "\"$_\"" } @old_rels).'], ';
        $command .= '['.join(', ', map { "\"$_\"" } @new_rels).'], ';
        $command .= "$permanent]}";

        $self->app->log->debug('REL',
            "Update manually SASL state files:");
        my %result  = $script->eval($controller, $command);
        unless ($result{'status'} && $result{'status'} eq 'ok') {
            $self->app->log->debug('REL', " failed.\n");
            return;
        }
        $self->app->log->debug('REL', ".\n");

        # Remove old releases.
        $self->app->log->debug('REL',
            "Remove old vanilla releases:");
        foreach my $rel (@old_rels) {
            if ($self->is_vanilla($rel)) {
                my $to_remove = File::Spec->catfile($rels_dir, $rel);
                if (-d $to_remove) {
                    $self->app->log->debug('REL', " $rel");
                    rmtree($to_remove);
                }
            }
        }
        $self->app->log->debug('REL', ".\n");
    } else {
        # The releases directory is freshly created (or will be), we
        # just copy files from default releases directory.
        $self->app->log->debug('REL',
            "Releases directory NOT populated: create\n");

        unless (-d $rels_dir || mkdir($rels_dir)) {
            $self->app->log->error(
                "Problem:\n",
                "  Releases directory '$rels_dir' isn't usable.\n",
                "  System reports:\n",
                "    $!\n");
            return;
        }

        # Copy releases.
        foreach my $rel (@default_rels) {
            my $ret = $self->_copy_vanilla_release($default_rels_dir, $rels_dir,
                $rel);
            return unless ($ret);
        }

        # Copy release_handler's state files.
        $self->app->log->debug('REL',
            "Copy release_handler's state files:");
        my @files = ('RELEASES', 'start_erl.data');
        for my $file (@files) {
            my $from = File::Spec->catfile($default_rels_dir, $file);
            my $to   = File::Spec->catfile($rels_dir, $file);

            if (-e $to) {
                $self->app->log->debug('REL', " ($file)");
                next;
            }

            $self->app->log->debug('REL', " $file");
            my $ret = copy($from, $to);
            unless ($ret) {
                $self->app->log->error(
                    "Problem:\n",
                    "  Failed to copy file '$file' from default releases\n",
                    "  directory to effective releases directory.\n",
                    "  Source and destination are:\n",
                    "    $from\n",
                    "    $to\n",
                    "  System reports:\n",
                    "    $!\n");
                $self->app->log->debug('REL', " failed.\n");
                return;
            }

            # Allow the user running the service to update this file.
            # This is mandatory for live upgrade to work.
            my $uid = $self->app->uid;
            my $gid = $self->app->gid;
            if (defined $uid || defined $gid) {
                $uid = -1 unless (defined $uid);
                $gid = -1 unless (defined $gid);
                $ret = chown $uid, $gid, $to;
                unless ($ret == 1) {
                    $self->app->log->warning(
                        "Problem:\n",
                        "  Failed to change owner of file '$file' in\n",
                        "  effective releases directory.\n",
                        "  System reports:\n",
                        "    $!\n");
                }
            }
        }
        $self->app->log->debug('REL', ".\n");
    }

    return 1;
}

sub _clear_releases_directory ($) {
    my ($self, $dir, $comment) = @_;

    # Remove vanilla releases.
    my $dh;
    unless (opendir($dh, $dir)) {
        $self->app->log->error(
            "Problem:\n",
            "  Failed to open releases directory '$dir'.\n",
            "  System reports:\n",
            "    $!\n");
        return;
    }

    $self->app->log->debug('REL',
        "Remove vanilla releases:");
    while (my $entry = readdir($dh)) {
        next if ($entry =~ /^\./o);
        if ($self->is_vanilla($entry)) {
            my $to_remove = File::Spec->catfile($dir, $entry);
            if (-d $to_remove) {
                $self->app->log->debug('REL', " $entry");
                rmtree($to_remove);
            }
        }
    }
    closedir($dh);
    $self->app->log->debug('REL', ".\n");

    # Remove release_handler's state files.
    $self->app->log->debug('REL',
        "Remove release_handler's state files:");
    my @files = ('RELEASES', 'start_erl.data');
    foreach my $file (@files) {
        my $to_remove = File::Spec->catfile($dir, $file);
        if (-f $to_remove) {
            $self->app->log->debug('REL', " $file");
            unlink $to_remove;
        }
    }
    $self->app->log->debug('REL', ".\n");
}

sub _list_vanilla_releases ($;$) {
    my ($self, $script, $node, $dir, $comment) = @_;

    my $RELEASES = File::Spec->catfile($dir, 'RELEASES');
    my $command  = "{erlsvc_release, list, [\"$RELEASES\"]}";
    my %result   = $script->eval($node, $command);
    unless ($result{'status'} && $result{'status'} eq 'ok') {
        return;
    }

    if ($comment) {
        $self->app->log->debug('REL',
            "Get vanilla releases list $comment:");
    } else {
        $self->app->log->debug('REL',
            "Get vanilla releases list:");
    }

    unless (defined $result{'return'}) {
        $self->app->log->debug('REL', " none.\n");
        return;
    }

    my @rels = ();
    foreach my $rel (keys %{$result{'return'}}) {
        if ($self->is_vanilla($rel)) {
            $self->app->log->debug('REL', " $rel");
            push @rels, $rel;
        }
    }
    $self->app->log->debug('REL', ".\n");

    return sort(@rels);
}

sub _copy_vanilla_release ($$$) {
    my ($self, $default_rels_dir, $rels_dir, $release) = @_;

    return 1 if ($rels_dir eq $default_rels_dir);

    $self->app->log->debug('REL',
        "Copy Erlang vanilla release '$release':");

    # Get ownership.
    my $uid = $self->app->uid;
    $uid = -1 unless (defined $uid);
    my $gid = $self->app->gid;
    $gid = -1 unless (defined $gid);

    # Create the release directory if necessary.
    my $erl_env = $self->app->erl_env;
    my $rel_dir = $erl_env->release_dir($release);
    my $default_rel_dir = $erl_env->default_release_dir($release);
    unless (-d $rel_dir || mkdir($rel_dir)) {
        $self->app->log->error(
            "Problem:\n",
            "  Release directory '$rel_dir' isn't usable.\n",
            "  System reports:\n",
            "    $!\n");
        $self->app->log->debug('REL', " failed.\n");
        return;
    }
    if ($uid != -1 && $gid != -1) {
        unless (chown($uid, $gid, $rel_dir)) {
            $self->app->log->error(
                "Problem:\n",
                "  Failed to set ownership on release directory '$rel_dir'.\n",
                "  System reports:\n",
                "    $!\n");
            $self->app->log->debug('REL', " failed.\n");
            return;
        }
    }

    # Copy the following files:
    #   - start_clean.*
    #   - start_sasl.*
    my @files = (
        'start.boot',
        'start.script',
        'start_clean.boot',
        'start_clean.rel',
        'start_clean.script',
        'start_sasl.boot',
        'start_sasl.rel',
        'start_sasl.script'
    );
    foreach my $file (@files) {
        my $from = File::Spec->catfile($default_rel_dir, $file);
        my $to   = File::Spec->catfile($rel_dir, $file);

        if (-e $to) {
            $self->app->log->debug('REL', " ($file)");
            next;
        }

        $self->app->log->debug('REL', " $file");
        my $ret = copy($from, $to);
        unless ($ret) {
            $self->app->log->error(
                "Problem:\n",
                "  Failed to copy file '$file' from default releases\n",
                "  directory to effective releases directory.\n",
                "  Source and destination are:\n",
                "    $from\n",
                "    $to\n",
                "  System reports:\n",
                "    $!\n");
            $self->app->log->debug('REL', " failed.\n");
            return;
        }
        if ($uid != -1 && $gid != -1) {
            unless (chown($uid, $gid, $to)) {
                $self->app->log->error(
                    "Problem:\n",
                    "  Failed to set ownership on file '$to'.\n",
                    "  System reports:\n",
                    "    $!\n");
                $self->app->log->debug('REL', " failed.\n");
                return;
            }
        }
    }

    $self->app->log->debug('REL', ".\n");

    return 1;
}

sub _grant_permissions () {
    my ($self) = @_;

    # The node may have to update files which don't belong to the
    # user/group running the script.
    my %chown = ();
    if ($self->app->user_changed || $self->app->group_changed) {
        my $uid = $self->app->uid;
        $uid = -1 unless (defined $uid);
        my $gid = $self->app->gid;
        $gid = -1 unless (defined $gid);

        my $erl_env = $self->app->erl_env;
        my @files = (
            $erl_env->start_erl_data,
            $erl_env->RELEASES
        );

        foreach my $file (@files) {
            my $stat = stat($file);

            if (($uid != -1 && $uid != $stat->uid) ||
                ($gid != -1 && $gid != $stat->gid)) {
                $self->app->log->debug('REL',
                    "Change owner of '$file' to $uid:$gid\n");

                my $ret = chown($uid, $gid, $file);

                unless ($ret) {
                    $self->app->log->warning(
                        "Failed to change ownership of the file '$file'.\n",
                        "The upgrade/reset may fail if the node can't update ".
                        "these files.\n",
                        "System reports:\n",
                        "  $!\n"
                    );

                    next;
                }

                $chown{$file} = [$stat->uid, $stat->gid];
            }
        }
    }

    return %chown;
}

sub _restore_permissions (%) {
    my ($self, %chown) = @_;

    foreach my $file (keys %chown) {
        my ($uid, $gid) = @{$chown{$file}};
        $self->app->log->debug('REL',
            "Restore owner of '$file' to $uid:$gid\n");
        my $ret = chown($uid, $gid, $file);

        unless ($ret) {
            $self->app->log->warning(
                "Failed to restore ownership of the file '$file'.\n",
                "You should restore them yourself; the owner was $uid:$gid.\n",
                "System reports:\n",
                "  $!\n"
            );
        }
    }
}

sub app () { shift->{'app'}; }

1;
