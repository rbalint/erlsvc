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

package ErlSvc::Ctl::Target;

use strict;
use warnings;
use utf8;

use File::Basename;
use File::Path qw(mkpath rmtree);
use File::Spec;
use YAML::Tiny;
use ErlSvc::Ctl::Proc;
require ErlSvc::Ctl::Release;

sub new ($) {
    my ($class, $app) = @_;
    $class = ref($class) || $class;

    my $self = {
        'app' => $app
    };

    bless $self => $class;
}

sub deploy (;%) {
    my ($self, %opts) = @_;

    my $target     = $self->app->node;
    my $target_dir = $opts{'directory'} || $self->_default_target_dir;

    $self->app->log->debug('TARGET',
        "<b>Deploy '$target' target system tree in $target_dir</b>\n");

    if (-e $target_dir && $opts{'from_scratch'}) {
        $self->destroy(%opts) or return;
    }

    # Create the directory if it doesn't exist yet.
    unless (-d $target_dir) {
        $self->app->log->debug('TARGET', "Create directory\n");

        my $errors;
        mkpath($target_dir, { 'error' => \$errors });

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
                "Problem:",
                "  Failed to create target system directory '$target_dir'",
                "  System reports:",
                $error_sub
            );

            return;
        }
    }

    my $erl_env  = $self->app->erl_env;
    my $root_dir = $erl_env->root_dir;

    my $proc = ErlSvc::Ctl::Proc->new($self->app);
    $proc->run_as_user($self->app->user, $self->app->group);
    $proc->reopen_stdio(0);

    # The directory is now created. We copy/update the files from the
    # system's Erlang environment that do not need any modification.
    $self->app->log->debug('TARGET',
        "Sync target directory with system root directory\n");
    my @cmdline = (
        'rsync', '-rlptD',
        '--exclude', 'COPYRIGHT',
        '--exclude', 'Install',
        '--exclude', 'PR.template',
        '--exclude', 'README',
        '--exclude', 'bin/dialyzer',
        '--exclude', 'bin/erlc',
        '--exclude', 'bin/run_test',
        '--exclude', 'bin/start',
        '--exclude', 'bin/typer',
        '--exclude', 'erts-*/doc',
        '--exclude', 'erts-*/include',
        '--exclude', 'erts-*/info',
        '--exclude', 'erts-*/lib',
        '--exclude', 'erts-*/man',
        '--exclude', 'erts-*/src',
        '--exclude', 'lib',
        '--exclude', 'doc',
        '--exclude', 'man',
        '--exclude', 'misc',
        '--exclude', 'releases/RELEASES',
        '--exclude', 'releases/start_erl.data',
        '--exclude', 'releases/R*',
        '--exclude', 'usr',
        $root_dir.'/', $target_dir.'/'
    );

    $proc->start(@cmdline);
    my $exit_code = $proc->wait_for_child;

    return if ($exit_code != 0);

    # The "lib" directory is copied in a second pass, because we don't
    # want to copy symlinks to external applications but the actual
    # application directory.
    return unless $self->_rsync_lib_dir($proc, $root_dir, $target_dir);

    # The bin/erl script contains an absolute path to the root
    # directory. We must update it to point to the target directory.
    my @dirs = ('bin');
    my $dh;
    unless (opendir($dh, $target_dir)) {
        $self->app->log->error(
            "Problem:\n",
            "  Failed to open target system directory '$target_dir'.\n",
            "  System reports:\n",
            "    $!\n");
        return;
    }

    while (my $entry = readdir($dh)) {
        if ($entry =~ /^erts-/o) {
            push @dirs, File::Spec->catfile($entry, 'bin');
        }
    }
    closedir($dh);

    foreach my $dir (@dirs) {
        my $erl_script = File::Spec->catfile($target_dir, $dir, 'erl');
        my $ret = $self->_update_erl_script($erl_script, $target_dir);

        return unless ($ret);
    }

    # Now that the node is deployed, we must reset the Erlang
    # environment.
    $erl_env->change_root_dir($target_dir) || return;
    $erl_env->change_releases_dir(
        File::Spec->catfile($target_dir, 'releases'));

    # The "releases" directory was not synchronized on purpose: we
    # delegate this action to ErlSvc::Ctl::Release.
    my $release = ErlSvc::Ctl::Release->new($self->app);
    $release->sync_vanilla_releases || return;

    # We write a hidden file in the target directory. This file is used
    # to known in other function if the root directory was created by
    # this function.
    my $fh;
    my $mark = $self->_mark_filename($target_dir);
    unless (open($fh, '>', $mark)) {
        $self->app->log->warning(
            "Problem:\n",
            "  Failed to mark the target system directorytarget as created\n",
            "  by this program.\n",
            "  System reports:\n",
            "    $!\n");
        return;
    }

    print $fh 'erlsvc_version: '.$ErlSvc::Ctl::VERSION."\n";
    print $fh 'source_root_dir:  "'.$root_dir."\"\n";
    close($fh);

    return 1;
}

sub sync_lib_dir (;%) {
    my ($self, %opts) = @_;

    unless ($self->is_target_system) {
        $self->app->log->error(
            "Problem:\n",
            "  This Erlang environment is not a target system created by\n",
            "  this program\n");
        return;
    }

    my $erl_env = $self->app->erl_env;
    my $target_dir = $erl_env->root_dir;

    my $source_dir = $opts{'source_dir'};
    unless ($source_dir) {
        my $mark = $self->_mark_filename($target_dir);
        my $yaml = YAML::Tiny->read($mark);
        unless ($yaml) {
            $self->app->log->error(
                "Problem:\n",
                "  Failed to read the target system's meta-data\n",
                "  The file is:\n",
                "    $mark\n",
                "  YAML::Tiny reports:\n",
                "    ".YAML::Tiny->errstr."\n");
            return;
        }

        $source_dir = $yaml->[0]->{'source_root_dir'};
    }

    unless (-d $source_dir) {
        $self->app->log->error(
            "Problem:\n",
            "  The source root directory isn't usable.\n",
            "  The directory is:\n",
            "    $source_dir\n",
            "  System reports:\n",
            "    $!\n");
        return;
    }

    my $proc = ErlSvc::Ctl::Proc->new($self->app);
    $proc->run_as_user($self->app->user, $self->app->group);
    $proc->reopen_stdio(0);

    $self->_rsync_lib_dir($proc, $source_dir, $target_dir);
}

sub _rsync_lib_dir ($$$) {
    my ($self, $proc, $source_dir, $target_dir) = @_;

    # The "src" sub-directory is copied because some applications, like
    # syslogger, need their source files.
    my @cmdline = (
        'rsync', '-rLptD',
        '--exclude', '*/doc',
        '--exclude', '*/examples',
        '--exclude', '*/info',
        File::Spec->catfile($source_dir, 'lib').'/',
        File::Spec->catfile($target_dir, 'lib').'/'
    );

    $proc->start(@cmdline);
    my $exit_code = $proc->wait_for_child;

    return ($exit_code == 0);
}

sub remove_unused () {
    my ($self, %opts) = @_;

    unless ($self->is_target_system) {
        $self->app->log->error(
            "Problem:\n",
            "  This Erlang environment is not a target system created by\n",
            "  this program\n");
        return;
    }

    my $target = $self->app->node;

    my $erl_env = $self->app->erl_env;
    my $target_dir = $erl_env->root_dir;

    $self->app->log->debug('TARGET',
        "Remove unused ERTS and applications from '$target' target ".
        "system tree in $target_dir\n");

    my $release  = ErlSvc::Ctl::Release->new($self->app);
    my %releases = $release->list;

    # We build a list of applications referenced by at least one
    # release. We build another list for ERTS versions.
    my %apps = ();
    my %ERTS = ();
    foreach my $rel (values (%releases)) {
        $ERTS{$rel->{'erts'}} = 1;

        foreach my $app (@{$rel->{'applications'}}) {
            my $name = $app->{'name'}.'-'.$app->{'version'};
            $apps{$name} = 1;
        }
    }

    # Remove unused ERTS.
    my $dh;
    unless (opendir($dh, $target_dir)) {
        $self->app->log->error(
            "Problem:\n",
            "  Failed to open target system directory '$target_dir'.\n",
            "  System reports:\n",
            "    $!\n");
        return;
    }

    $self->app->log->debug('TARGET', "Remove unused ERTS:");
    while (my $entry = readdir($dh)) {
        next unless ($entry =~ /^erts-([0-9.]+)/o);
        next if ($ERTS{$1});

        # This version of ERTS is not used anymore: remove it.
        $self->app->log->debug('TARGET', " $1");
        my $dir = File::Spec->catfile($target_dir, "erts-$1");
        if ($opts{'dry_run'}) {
            $self->app->log->info("Remove ERTS $1 ($dir)\n");
            next;
        }

        my $errors = [];
        rmtree($dir, { 'error' => \$errors });

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
            $self->app->log->warning(
                "Problem:\n",
                "  Failed to remove ERTS $1 directory".
                "'$dir'\n",
                "  System reports:\n",
                $error_sub
            );
        }
    }
    closedir($dh);
    $self->app->log->debug('TARGET', ".\n");

    # Remove unused applications.
    my $lib_dir = File::Spec->catfile($target_dir, 'lib');
    unless (opendir($dh, $lib_dir)) {
        $self->app->log->error(
            "Problem:\n",
            "  Failed to open target system directory '$target_dir'.\n",
            "  System reports:\n",
            "    $!\n");
        return;
    }

    $self->app->log->debug('TARGET', "Remove unused applications:");
    while (my $entry = readdir($dh)) {
        next if ($entry =~ /^\./o);
        next if ($apps{$entry});

        # Keep Erlang modules associated with used ERTS.
        if ($entry =~ /^erts-([0-9.]+)/o) {
            next if ($ERTS{$1});
        }

        # This application is not used anymore: remove it.
        $self->app->log->debug('TARGET', " $entry");
        my $dir = File::Spec->catfile($lib_dir, $entry);
        if ($opts{'dry_run'}) {
            $self->app->log->info("Remove application $entry ($dir)\n");
            next;
        }

        if (-l $dir) {
            unless (unlink $dir) {
                $self->app->log->warning(
                    "Problem:\n",
                    "  Failed to remove application $entry directory".
                    "'$dir'\n",
                    "  System reports:\n",
                    "    $!\n");
            }
        } else {
            my $errors = [];
            rmtree($dir, { 'error' => \$errors });

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
                $self->app->log->warning(
                    "Problem:\n",
                    "  Failed to remove application $entry directory".
                    "'$dir'\n",
                    "  System reports:\n",
                    $error_sub
                );
            }
        }
    }
    closedir($dh);
    $self->app->log->debug('TARGET', ".\n");
}

sub destroy (;%) {
    my ($self, %opts) = @_;

    my $target = $self->app->node;
    if ($target->is_alive && !$target->autostarted) {
        $self->app->log->error(
            "Problem:\n",
            "  The node '$target' is already running.\n"
        );

        return;
    }

    my $target_dir = $opts{'directory'} || $self->_default_target_dir;

    $self->app->log->debug('TARGET',
        "<b>Remove content of target directory $target_dir</b>\n");

    my $errors;
    rmtree($target_dir, { 'keep_root' => 1, 'error' => \$errors });

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
            "Problem:",
            "  Failed to remove target system files in directory ".
            "'$target_dir'",
            "  System reports:",
            $error_sub
        );

        return;
    }

    return 1;
}

sub is_target_system () {
    my ($self) = @_;

    my $erl_env  = $self->app->erl_env;
    my $root_dir = $erl_env->root_dir;

    my $mark = $self->_mark_filename($root_dir);

    return (-e $mark);
}

sub _update_erl_script ($$) {
    my ($self, $erl_script, $target_dir) = @_;

    $self->app->log->debug('TARGET', "Update ROOTDIR in $erl_script\n");

    # Read the file and update ROOTDIR.
    my $fh;
    unless (open($fh, '<', $erl_script)) {
        $self->app->log->error(
            "Problem:\n",
            "  Failed to open erl script '$erl_script' for reading.\n",
            "  System reports:\n",
            "    $!\n");
        return;
    }
    my @lines = ();
    foreach my $line (<$fh>) {
        if ($line =~ /^(\s*ROOTDIR=\s*)/o) {
            push @lines, "$1$target_dir\n";
        } else {
            push @lines, $line;
        }
    }
    close($fh);

    # Write the new content, overwriting the previous one.
    unless (open($fh, '>', $erl_script)) {
        $self->app->log->error(
            "Problem:\n",
            "  Failed to open erl script '$erl_script' for writing.\n",
            "  System reports:\n",
            "    $!\n");
        return;
    }
    print $fh @lines;
    close($fh);

    return 1;
}

sub _default_target_dir () {
    my ($self) = @_;

    my $home   = $self->app->home;
    my $target = $self->app->node;

    # Default directory is:
    #   $HOME/target-systems/$node@$host
    my $target_dir = File::Spec->catfile($home, 'target-systems',
        $target->full_node_name);

    return $target_dir;
}

sub _mark_filename ($) {
    my ($self, $root_dir) = @_;

    File::Spec->catfile($root_dir, '.erlsvc_target_system.yaml');
}

sub app () { shift->{'app'}; }

1;
