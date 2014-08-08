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

package ErlSvc::Ctl::Service;

use strict;
use warnings;
use utf8;

use Cwd qw(getcwd abs_path);
use Data::Dumper;
use File::Spec;
use File::Temp qw(tempfile);
use POSIX qw(strftime);
use Sys::CPU;
use Time::HiRes qw(usleep);
use ErlSvc::Ctl::Erlang::Script;
use ErlSvc::Ctl::Proc;
use ErlSvc::Ctl::Target;
use ErlSvc::Ctl::Usage qw(abs_progname);

sub new ($) {
    my ($class, $app) = @_;
    $class = ref($class) || $class;

    my $self = {
        'app' => $app
    };

    bless $self => $class;
}

sub name () {
    'My service';
}

sub start (;%) {
    my ($self, %opts) = @_;

    my $node = $self->app->node;
    my $target_system = ErlSvc::Ctl::Target->new($self->app);
    my $is_target_system = $target_system->is_target_system;

    # Check that the node is local and not already started. For a local
    # node, if the service is already running, it's ok. Otherwise, we
    # report an error.
    #
    # Starting a remote node would require an SSH access.
    if (!$node->is_local)  {
        $self->app->log->error(
            "Problem:\n",
            "  The node '$node' is remote.\n"
        );

        return;
    } elsif ($node->is_alive) {
        if ($self->is_running) {
            # The service is already running.
            return 2;
        }

        $self->app->log->error(
            "Problem:\n",
            "  The node '$node' is already started but the service\n",
            "  is NOT running.\n",
            "\n",
            "Solution(s):\n",
            "  1. Check that an Erlang shell using the same node name\n",
            "     is not running.\n"
        );

        return;
    }

    $self->app->log->debug("SERV", "<b>Starting service on node '$node'</b>\n");

    # Set default options.
    $opts{'embedded'}  = 1     unless (exists $opts{'embedded'});
    $opts{'max_users'} = 10000 unless ($opts{'max_users'});

    # If the called only wants to open a shell with sys.config
    # specified, don't try to start an embedded node.
    delete $opts{'embedded'} if ($opts{'load_only'});

    my $erl_env = $self->app->erl_env;

    # The file start_erl.data is used by run_erl(1) to know which
    # release to boot.
    my $start_erl_data = $erl_env->start_erl_data;
    my $start_erl_data_is_tmp = 0;

    # Regardless of start_erl.data content, the caller may specify a
    # release. When the embedded mode is selected, we'll have to create
    # a fake start_erl.data indicating this specific release.
    my $release = $self->app->opts('release');

    # If the start_erl.data file doesn't exist and if no release was
    # specified, we can't determine the release to boot.
    unless ($release || -f $start_erl_data) {
        $self->app->log->error(
            "Problem:\n",
            "  Failed to determine the release to boot.\n",
            "  The start_erl.data file '$start_erl_data' doesn't exist.\n",
            "\n",
            "Solution(s):\n",
            "  1. Check that the releases directory is correct.\n",
            "  2. Specify a release explicitly by using the\n",
            "     global -r flag.\n"
        );

        return;
    }

    # If the start_erl.data file doesn't exist or if the explicitly
    # specified release differs from the default release, we must create
    # a temporary start_erl.data overriding the default release.

    if ($opts{'embedded'}) {
        # In embedded mode, the start_erl.data file is required by
        # run_erl(1). If it doesn't exist, we must create a fake one.
        #
        # Likewise, if the specified release differs from the one
        # indicated by start_erl.data, we must create a fake one to
        # change the release.

        my ($erts, $default_release);
        if (-f $start_erl_data) {
            ($erts, $default_release) = $erl_env->parse_start_erl_data(
                $start_erl_data);

            # If the release was not specified, we select the default
            # release.
            $release = $default_release unless ($release);
        }

        if (!$default_release || $release ne $default_release) {
            # We must create the fake start_erl.data file.

            $self->app->log->debug("SERV",
                'Preare a fake start_erl.data ',
                $default_release ?
                    "(release $release != default release $default_release)\n" :
                    "(file '$start_erl_data' doesn't exist)\n"
            );

            # This file contains a single line with two fields: ERTS
            # version and the default release. If we coudn't get ERTS
            # version because start_erl.data doesn't exist, we query it.
            unless ($erts) {
                $erts = $erl_env->erts_version;
            }

            my $fh;
            my $template = File::Spec->catfile($ENV{'HOME'},
                'start_erl.data.XXXX');
            ($fh, $start_erl_data) = tempfile($template);
            print $fh "$erts $release\n";
            close($fh);

            # This temporary file will be removed after the node start.
            $start_erl_data_is_tmp = 1;
        }
    } else {
        # In shell mode, start_erl.data isn't required. But if the
        # release was not specified, we need this file to get the
        # default one.

        unless ($release) {
            my ($erts, $default_release) = $erl_env->parse_start_erl_data(
                $start_erl_data);

            # If the release was not specified, we select the default
            # release.
            $release = $default_release;
        }
    }

    if ($opts{'embedded'}) {
        my $service_name = $self->name;
        $self->app->log->info("Starting $service_name ($release):");
        $self->app->log->waiting(1);
    }

    $self->app->log->debug("SERV", "Release: $release\n");
    $self->app->log->debug("SERV", "start_erl.data: '$start_erl_data'\n");

    # Now that we know the release, we determine the sys.config file.
    my $sysconfig = $erl_env->release_sysconfig($release);

    unless (-f $sysconfig) {
        # The sys.config file doesn't exist but it's required for the
        # node to start.
        my $error = $!;

        if ($start_erl_data_is_tmp) {
            # Remove the temporary start_erl.data file.
            $self->app->log->debug("SERV",
                "Remove temporary start_erl.data '$start_erl_data'\n");
            unlink $start_erl_data;
        }

        $self->app->log->error(
            "Problem:\n",
            "  sys.config file '$sysconfig' isn't readable.\n",
            "  The node won't start without it.\n",
            "  System reports:\n",
            "    $error\n"
        );

        $self->app->log->info(" failed\n") if ($opts{'embedded'});
        return;
    }

    $sysconfig = abs_path($sysconfig);
    $self->app->log->debug("SERV", "sys.config: '$sysconfig'\n");

    # We can determine the boot script too.
    my $boot_script = $erl_env->release_boot_script($release);

    unless (-f $boot_script || $opts{'load_only'}) {
        # The boot script file doesn't exist but it's required for the
        # node to start.
        my $error = $!;

        if ($start_erl_data_is_tmp) {
            # Remove the temporary start_erl.data file.
            $self->app->log->debug("SERV",
                "Remove temporary start_erl.data '$start_erl_data'\n");
            unlink $start_erl_data;
        }

        $self->app->log->error(
            "Problem:\n",
            "  boot script file '$boot_script' isn't readable.\n",
            "  The node won't start without it.\n",
            "  System reports:\n",
            "    $error\n"
        );

        $self->app->log->info(" failed\n") if ($opts{'embedded'});
        return;
    }

    $boot_script = abs_path($boot_script);
    $self->app->log->debug("SERV", "boot script: '$boot_script'\n");

    # At this stage, we have the release name, a sys.config file and a
    # boot script.

    # Compute settings from the expected maximum number of users.
    my $erl_max_ports = exists $opts{'ERL_MAX_PORTS'} ?
        $opts{'ERL_MAX_PORTS'} :
        $opts{'max_users'} * 2;
    my $erl_max_ets_tables = exists $opts{'ERL_MAX_ETS_TABLES'} ?
        $opts{'ERL_MAX_ETS_TABLES'} :
        $opts{'max_users'} * 2;
    my $max_processes = exists $opts{'max_processes'} ?
        $opts{'max_processes'} :
        $opts{'max_users'} * 4;

    # Compute settings from hardware informations.
    my $cpu_count     = Sys::CPU::cpu_count();
    my $async_threads = $cpu_count * 4;

    # Prepare environment.
    my %export_env = (
        'ERL_MAX_PORTS'      => $erl_max_ports,
        'ERL_MAX_ETS_TABLES' => $erl_max_ets_tables,
        'WCTL_VERSION'       => $ErlSvc::Ctl::VERSION
    );

    if ($self->app->opts('erllibs_dir')) {
        my @erllibs = @{$self->app->opts('erllibs_dir')};
        $export_env{'ERL_LIBS'} = join(':', @erllibs);
    }

    if ($self->app->opts('releases_dir')) {
        # Set SASL's releases directory. We use an environment variable
        # instead of the application parameter to keep node command line
        # as simple as possible.
        my $rels_dir = $self->app->opts('releases_dir');
        $export_env{'RELDIR'} = $rels_dir;
    }

    # We put the options given to the start function in the environment
    # too. This is later used by restart.
    my $serialized;
    {
        local $Data::Dumper::Indent = 0;
        local $Data::Dumper::Purity = 1;
        $serialized = Dumper(\%opts);
    }
    $export_env{'WCTL_START_OPTS'} = $serialized;

    if ($opts{'embedded'}) {
        # Change default log rotation for run_erl(1) log files
        # (erlang.log.$N).
        unless (exists $ENV{'RUN_ERL_LOG_GENERATIONS'}) {
            $export_env{'RUN_ERL_LOG_GENERATIONS'} = 10;
        }
        unless (exists $ENV{'RUN_ERL_LOG_MAXSIZE'}) {
            $export_env{'RUN_ERL_LOG_MAXSIZE'} = 10 * 1024 * 1024;
        }
    }

    # Initialize the flags.
    my @cmdline = ();

    my @flags = (
        '+Ww', '+K', 'true',
        '+A', "$async_threads",
        '+P', "$max_processes",
        '-sname', $node->node_name,
    );
    if ($opts{'embedded'}) {
        unshift @flags, '+Bi';
    }
    if ($node->cookie) {
        push @flags, ('-setcookie', $node->cookie);
    }

    # Setup heart(1).
    $self->app->log->debug("SERV", 'Heart monitoring: '.
        ($opts{'heart'} ? 'enabled' : 'disabled')."\n");
    if ($opts{'heart'}) {
        push @flags, '-heart';

        my $heart_cmd;
        if ($opts{'restart_cmd'} && $opts{'embedded'}) {
            $export_env{'HEART_COMMAND'} = $opts{'restart_cmd'};
            $heart_cmd = $opts{'restart_cmd'};
        } else {
            $heart_cmd = ($opts{'embedded'}) ?
                '(none specified)' :
                '(none in shell mode)';
        }

        $self->app->log->debug("SERV", "Heart command: $heart_cmd\n");
    }

    # SASL start_prg can't have argument. Therefore, we set an
    # environment variable and erlsvc will launch the command found in
    # this variable.
    $export_env{'WCTL_SASL_START_PRG'} = $opts{'restart_cmd'};
    push @flags, (
        '-sasl', 'start_prg', '"'.abs_progname().'"'
    );

    # Append Erlang application args.
    my $erlapp_args = $opts{'erlapp_args'} ||
      $self->app->opts('erlapp_args');
    if ($erlapp_args && ref($erlapp_args) eq 'HASH') {
        while (my ($app, $args) = each %$erlapp_args) {
            while (my ($arg, $value) = each %$args) {
                push @flags, ("-$app", $arg, $value);
            }
        }
    }

    # Append extra flags.
    my $extra_flags = $opts{'extra_flags'} || $self->app->opts('extra_flags');
    if ($extra_flags && ref($extra_flags) eq 'ARRAY') {
        push @flags, @$extra_flags;
    }

    if ($opts{'embedded'}) {
        # Setup an embedded node using run_erl(1).
        $self->app->log->debug("SERV", "Execution mode: embedded\n");

        my $erl_cmd = $erl_env->erl_cmd('cmd' => 'run_erl');
        push @cmdline, ($erl_cmd, '-daemon');

        # run_erl(1) needs a directory to put its pipe and its log files.
        my $pipe_dir = $self->app->opts('pipe_dir');
        unless (-d $pipe_dir || mkdir($pipe_dir)) {
            # The pipe directory file doesn't exist.
            my $error = $!;

            if ($start_erl_data_is_tmp) {
                # Remove the temporary start_erl.data file.
                $self->app->log->debug("SERV",
                    "Remove temporary start_erl.data '$start_erl_data'\n");
                unlink $start_erl_data;
            }

            $self->app->log->error(
                "Problem:\n",
                "  The run directory '$pipe_dir' is invalid.\n",
                "  It's required by run_erl(1).\n",
                "  System reports:\n",
                "    $error\n"
            );

            $self->app->log->info(" failed\n");
            return;
        }
        $pipe_dir = abs_path($pipe_dir).'/';
        push @cmdline, $pipe_dir;

        my $log_dir = $self->app->opts('log_dir');
        unless (-d $log_dir || mkdir($log_dir)) {
            # The log directory file doesn't exist.
            my $error = $!;

            if ($start_erl_data_is_tmp) {
                # Remove the temporary start_erl.data file.
                $self->app->log->debug("SERV",
                    "Remove temporary start_erl.data '$start_erl_data'\n");
                unlink $start_erl_data;
            }

            $self->app->log->error(
                "Problem:\n",
                "  The log directory '$log_dir' is invalid.\n",
                "  It's required by run_erl(1).\n",
                "  System reports:\n",
                "    $error\n"
            );

            $self->app->log->info(" failed\n");
            return;
        }
        $log_dir = abs_path($log_dir);
        push @cmdline, $log_dir;

        my $start_erl = abs_path($erl_env->erl_cmd('cmd' => 'start_erl'));
        my $root_dir  = abs_path($erl_env->root_dir);
        my $rels_dir  = abs_path($erl_env->releases_dir);
        my $command   = "exec $start_erl $root_dir $rels_dir $start_erl_data";

        # When adding additionnal flags, we must escape quotes and
        # double-quotes.
        my @flags_escaped = map { $_ =~ s/(["' ])/\\$1/g; $_; } @flags;
        $command .= ' '.join(' ', @flags_escaped);
        push @cmdline, $command;
    } else {
        # Start the service in a standard shell using erl(1).
        $self->app->log->debug("SERV", "Execution mode: shell\n");

        my $erl_cmd = $erl_env->erl_cmd();
        push @cmdline, $erl_cmd;

        push @cmdline, @flags;

        unless ($opts{'load_only'}) {
            # Unless load_only was specified, we indicate which boot
            # script to use.
            #
            # On erl(1) command line, the boot script file must not have
            # its file extension.
            my $boot_script_stripped = $boot_script;
            $boot_script_stripped =~ s/\.boot$//o;
            push @cmdline, ('-boot', $boot_script_stripped);
        }

        # On erl(1) command line, the sys.config file must not have its
        # file extension.
        my $sysconfig_stripped = $sysconfig;
        $sysconfig_stripped =~ s/\.config$//o;
        push @cmdline, ('-config', $sysconfig_stripped);
    }

    my $debug_sub = sub {
        my @output = ("Final node command line:\n");
        my $line = '';
        foreach my $arg (@cmdline) {
            if (length($line) + length($arg) > 65) {
                push @output, " $line \\\n";
                $line = $arg;
            } elsif (!$line) {
                $line = $arg;
            } else {
                $line .= ' '.$arg;
            }
        }
        push @output, " $line\n";
        return @output;
    };
    $self->app->log->debug("SERV", $debug_sub);

    # Run as a child process.
    my $proc = ErlSvc::Ctl::Proc->new($self->app);
    $proc->run_as_user($self->app->user, $self->app->group);
    $proc->set_open_files_limit(131072);
    $proc->set_env(%export_env);
    $proc->reopen_stdio(0);

    # The working directory is the effective user home directory by
    # default. But if the Erlang environment is a target system, we use
    # this environment's root directory.
    $proc->set_working_dir($erl_env->root_dir) if ($is_target_system);

    # In this working directory, we backup old "erl_crash.dump" if it's present.
    my $wd = $proc->working_dir || getcwd;
    my $erl_crash_dump = File::Spec->catfile($wd, 'erl_crash.dump');
    if (-e $erl_crash_dump) {
        my $old_erl_crash_dump = File::Spec->catfile($wd,
            strftime("erl_crash-%Y%m%d-%H%M%S.dump", localtime()));
        $self->app->log->debug("SERV",
            "Rename old erl_crash.dump to '$old_erl_crash_dump'\n");
        unless (rename($erl_crash_dump, $old_erl_crash_dump)) {
            $self->app->log->warning(
                "Problem:\n",
                "  Failed to rename old erl_crash.dump to ".
                "'$old_erl_crash_dump'\n",
                "  System reports:\n",
                "    $!\n",
            );
        }
    }

    unless ($opts{'embedded'}) {
        # Unhide cursor.
        $self->app->log->show_cursor(1);
    }

    unless ($proc->start(@cmdline)) {
        $proc->wait_for_child;

        if ($start_erl_data_is_tmp) {
            # Remove the temporary start_erl.data file.
            $self->app->log->debug("SERV",
                "Remove temporary start_erl.data '$start_erl_data'\n");
            unlink $start_erl_data;
        }

        $self->app->log->error(
            "Problem:\n",
            "  Failed to start service on node '$node'.\n",
            "  Check erlang.log.* files in the logs directory.\n",
        );

        $self->app->log->info(" failed\n") if ($opts{'embedded'});
        return;
    }

    my $ret = 1;
    if ($opts{'embedded'}) {
        # run_erl(1) returns before the node is fully started. Therefore
        # we use an Erlang script which monitors applications start.
        # This allows this function to block until the node is ready.

        my $controller = $self->app->controller;
        unless ($controller->set_target($node)) {
            if ($start_erl_data_is_tmp) {
                # Remove the temporary start_erl.data file.
                $self->app->log->debug("SERV",
                    "Remove temporary start_erl.data '$start_erl_data'\n");
                unlink $start_erl_data;
            }

            $self->app->log->info(" failed\n");
            return;
        }

        my $script  = ErlSvc::Ctl::Erlang::Script->new($self->app);
        my $verbose = $self->app->verbose('SERV') ? 'true' : 'false';
        my $command = "{erlsvc_service, watch_start, [[{verbose, $verbose}]]}";

        my %result = $script->eval($controller, $command);
        $self->app->log->waiting(0);

        if ($result{'status'} && $result{'status'} eq 'ok') {
            $self->app->log->debug("SERV",
                "Service on node '$node' is up and running\n");
        } else {
            $self->stop('force' => 1);
            $ret = 0;
        }
    } else {
        # Wait for the shell to exit.
        $proc->wait_for_child;

        # Hide cursor.
        $self->app->log->show_cursor(0);
    }

    if ($start_erl_data_is_tmp) {
        # Remove the temporary start_erl.data file.
        $self->app->log->debug("SERV",
            "Remove temporary start_erl.data '$start_erl_data'\n");
        unlink $start_erl_data;
    }

    return $ret;
}

sub is_running () {
    my ($self) = @_;

    # Get the target node and check he's running.
    my $target = $self->app->node;

    if (!$target->is_alive) {
        # The node is down, the service too.
        return 0;
    }

    # The command will run on the target node but we use the controller
    # node as a gateway.
    my $controller = $self->app->controller;
    $controller->set_target($target) or return;

    my $script = ErlSvc::Ctl::Erlang::Script->new($self->app);

    $self->app->log->debug("SERV",
        "<b>Checking service status on node '$target'</b>\n");

    my $command = "{erlsvc_service, is_running, []}";

    my %result = $script->eval($controller, $command);
    unless ($result{'status'} && $result{'status'} eq 'ok') {
        return;
    }

    my $is_up = ($result{'return'} eq 'true') ? 1 : 0;
    $self->app->log->debug("SERV",
        "Service on node '$target' is ".($is_up ? 'UP' : 'DOWN')."\n");
    return $is_up;
}

sub reload () {
    my ($self) = @_;

    my $target = $self->app->node;

    unless ($target->is_alive) {
        $self->app->log->error(
            "Problem:\n",
            "  The node '$target' is not running.\n"
        );

        return;
    }

    my $controller = $self->app->controller;
    $controller->set_target($target) or return;

    $self->app->log->debug("SERV",
        "<b>Reload configuration on node '$target'</b>\n");

    my $script  = ErlSvc::Ctl::Erlang::Script->new($self->app);
    my $command = "{erlsvc_service, reload_config, []}";
    my %result  = $script->eval($controller, $command);
    unless ($result{'status'} && $result{'status'} eq 'ok') {
        return;
    }

    return 1;
}

sub add_start_opts_of_running_node ($)
{
    my ($self, $opts) = @_;

    my $target = $self->app->node;

    # If the target node is down, we obviously can't query the start
    # options.
    return unless ($target->is_alive);

    $self->app->log->debug('SERV', "Retrieve previous start options\n");

    my $controller = $self->app->controller;
    $controller->set_target($target) or return;

    my $script  = ErlSvc::Ctl::Erlang::Script->new($self->app);
    my $command = "{erlsvc_service, get_start_opts, []}";
    my %result  = $script->eval($controller, $command);
    unless ($result{'status'} && $result{'status'} eq 'ok') {
        return;
    }

    my $VAR1;
    eval $result{'return'};
    my %old_opts = %$VAR1;

    # Let's merge options.
    while (my ($k, $v) = each %old_opts) {
        unless (exists $opts->{$k}) {
            $self->app->log->debug('SERV',
                "Use previous value for $k\n");
            $opts->{$k} = $v;
        }
    }
}

sub restart (;%) {
    my ($self, %opts) = @_;

    my $target = $self->app->node;

    # We can't restart a remote node.
    if (!$target->is_local) {
        $self->app->log->error(
            "Problem:\n",
            "  The node '$target' is remote.\n"
        );

        return;
    } elsif ($target->is_alive && !$self->is_running) {
        # The node is up but the service is not running. We don't touch
        # a node we don't know.

        $self->app->log->error(
            "Problem:\n",
            "  The node '$target' is started but the service is NOT\n",
            "  running.\n",
            "\n",
            "Solution(s):\n",
            "  1. Check that an Erlang shell using the same node name\n",
            "     is not running.\n"
        );

        return;
    }

    if ($target->is_alive) {
        # The node is already started. Here's the plan:
        #   - query the start options
        #   - stop the node
        #   - start it again

        $self->add_start_opts_of_running_node(\%opts);

        $self->app->log->debug("SERV",
            "<b>Restart service on node '$target'</b>\n");

        # We're ready to stop the service.
        my $ret = $self->stop(%opts);
        return unless ($ret);
    } else {
        # The node is not started. We simply start it.
        $self->app->log->debug("SERV",
            "<b>Restart service on node '$target'</b>\n");
        $self->app->log->debug('SERV', "Service already stopped\n");
    }

    # At this point, the service is stopped and %opts contains the final
    # options.
    $self->start(%opts);
}

sub stop (;%) {
    my ($self, %opts) = @_;

    my $target = $self->app->node;

    # Check that the node is already started. If the service is not
    # running, it's ok, we return immediately.
    if (!$opts{'force'} && !$target->is_alive) {
        # The node is down, there's nothing to do. Really nothing...
        # I'd give anything to... I don't know... Go to the beach! I've
        # never seen the ocean :( I can't even see: as a program, I
        # have no eyeballs! Life is awful :( But... I... I don't live!
        # AAAAAAHHHH!

        return 2;
    } elsif (!$opts{'force'} && !$self->is_running) {
        # The node is here but it doesn't run the service. This node
        # belongs to someone else. It's none of my business.

        $self->app->log->error(
            "Problem:\n",
            "  The node '$target' is up but it doesn't run the service.\n",
            "\n",
            "Solution(s):\n",
            "  1. Check that an Erlang shell using the same node name\n",
            "     is not running.\n"
        );

        return;
    }

    $self->app->log->debug("SERV",
        "<b>Stopping service on node '$target'</b>\n");

    my $service_name = $self->name;
    $self->app->log->info("Stopping $service_name:");
    $self->app->log->waiting(1);

    # Set default options.
    $opts{'timeout'} = 30 unless (exists $opts{'timeout'});

    my $controller = $self->app->controller;
    $controller->set_target('self');

    # To stop the service, we just send an init:stop() to the node.
    # After that, the script will watch the node. This way, we'll be
    # notified when the service will be really down.

    my $timeout = $opts{'timeout'} * 1000;
    my $force   = $opts{'force'} ? 'true' : 'false';
    my $verbose = $self->app->verbose('SERV') ? 'true' : 'false';

    my $script  = ErlSvc::Ctl::Erlang::Script->new($self->app);
    my $command = "{erlsvc_service, stop, ['$target', ".
        "[{timeout, $timeout}, {force, $force}, {verbose, $verbose}]]}";

    my %result = $script->eval($controller, $command);
    $self->app->log->waiting(0);

    unless ($result{'status'} && $result{'status'} eq 'ok') {
        return;
    }

    $self->app->log->info(' '.$result{'return'}.".\n");
    if ($result{'return'} eq 'failed') {
        return;
    }

    $self->app->log->debug("SERV",
        "Service on node '$target' is stopped\n");
    return 1;
}

sub shell (;%) {
    my ($self, %opts) = @_;

    my $target = $self->app->node;

    unless ($target->is_alive) {
        $self->app->log->error(
            "Problem:\n",
            "  The node '$target' is not running.\n"
        );

        return;
    }

    my $mode = $opts{'mode'};
    if (!$target->is_local && $mode && $mode eq 'to_erl') {
        # We can't use to_erl(1) to connect to a remote node.
        $self->app->log->error(
            "Problem:\n",
            "  Can't use to_erl(1) to open a shell on a remote.\n"
        );

        return;
    }

    unless ($mode) {
        # Default mode is "to_erl" for local node, "remsh" otherwise.
        $mode = ($target->is_local) ? 'to_erl' : 'remsh';
    }

    # Stop the controller node, because he could receive the ^C too.
    my $controller = $self->app->controller('no_init' => 1);
    $controller->stop if (defined $controller);

    my $ret;
    if ($mode eq 'to_erl') {
        $ret = $self->_shell_to_erl;
    }
    if (!$ret && !$opts{'mode'}) {
        if ($mode eq 'to_erl') {
            $self->app->log->warning("Fallback to \"remsh\" mode\n");
        }
        $ret = $self->_shell_remsh;
    }
    return $ret;
}

sub _shell_to_erl () {
    my ($self) = @_;

    my $target = $self->app->node;

    my $erl_env    = $self->app->erl_env;
    my $to_erl_cmd = $erl_env->erl_cmd('cmd' => 'to_erl');
    my $pipe_dir   = $self->app->opts('pipe_dir');

    # We want to send ^L to run_erl(1) to force it to redraw. Thanks
    # to this, the prompt will be displayed. To send this ^L, we need
    # the name of the write side of the named pipe: we walk through the
    # $pipe_dir and look for the erlang.pipe.*.w entries. We take all
    # pipes because we don't know which one will be used.
    #
    # TODO: If there're dead pipes lying in this directory, writing ^L
    # will block this process.
    my @pipes = glob(File::Spec->catfile($pipe_dir, 'erlang.pipe.*.w'));

    my $proc = ErlSvc::Ctl::Proc->new($self->app);
    $proc->reopen_stdio(0);

    my @cmdline = (
        $to_erl_cmd,
        $pipe_dir.'/'
    );

    $self->app->log->debug('SERV', "Start a shell (mode: to_erl)\n");

    $self->app->log->info(
        "\n",
        "  ---------------------------------\n",
        "   To close this shell, hit Ctrl+D\n",
        "  ---------------------------------\n",
        "\n"
    );

    # Restore cursor.
    $self->app->log->show_cursor(1);

    $proc->start(@cmdline);

    # Now that to_erl(1) is running, send the ^L. We sleep 100 ms to let
    # to_erl(1) some time to negotiate with run_erl(1).
    usleep(100000);
    my $fh;
    foreach my $pipe (@pipes) {
        $self->app->log->debug('SERV',
            "Send ^L to pipe $pipe\n");

        unless (open($fh, '>'.$pipe)) {
            $self->app->log->warning(
                "Failed to open pipe $pipe: $!\n");
            next;
        }
        print $fh "";
        close($fh);
    }

    my $exit_code = $proc->wait_for_child;

    # Hide cursor again.
    $self->app->log->show_cursor(0);

    return ($exit_code == 0) ? 1 : 0;
}

sub _shell_remsh () {
    my ($self) = @_;

    my $target = $self->app->node;

    my $erl_env = $self->app->erl_env;
    my $erl_cmd = $erl_env->erl_cmd;

    my $proc = ErlSvc::Ctl::Proc->new($self->app);
    $proc->reopen_stdio(0);

    my @cmdline = (
        $erl_cmd,
        '-sname', "erlsvc-remsh-$$",
        '-remsh', $target->full_node_name,
        '-hidden'
    );

    if ($target->cookie) {
        push @cmdline, ('-setcookie', $target->cookie);
    }

    $self->app->log->debug('SERV', "Start a shell (mode: remsh)\n");

    $self->app->log->info(
        "\n",
        "  ---------------------------------------\n",
        "   To close this shell, hit Ctrl+C twice\n",
        "  ---------------------------------------\n",
        "\n"
    );

    # Restore cursor.
    $self->app->log->show_cursor(1);

    $proc->start(@cmdline);
    my $exit_code = $proc->wait_for_child;

    # Hide cursor again.
    $self->app->log->show_cursor(0);

    return ($exit_code == 0) ? 1 : 0;
}

sub app () { shift->{'app'}; }

1;
