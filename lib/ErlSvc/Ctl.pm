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

package ErlSvc::Ctl;
use base qw(CLI::Framework ErlSvc::Ctl::Resources);

use 5.010000;

use strict;
use warnings;
use utf8;

our $VERSION = '1.00';

use Cwd qw(abs_path);
use File::ShareDir qw(dist_dir);
use YAML::Tiny;
use ErlSvc::Ctl::Log;
use ErlSvc::Ctl::Usage qw(progname abstract);
use ErlSvc::Ctl::Service;
use ErlSvc::Ctl::Erlang::Env;
use ErlSvc::Ctl::Erlang::Node;
use ErlSvc::Ctl::Exceptions qw(:all);

sub init {
    my ($self, $opts) = @_;

    # Save global options.
    $self->cache->set('global_opts' => $opts);

    # Set default configuration.
    my $default_mods_dir = eval { dist_dir('erlsvc') };
    my %default_config = (
        'node'         => 'myservice',
        'host'         => ErlSvc::Ctl::Erlang::Node::local_hostname(),
        'mods_dir'     => $default_mods_dir,
        'pipe_dir'     => '/var/run/my_service',
        'log_dir'      => '/var/log/my_service'
    );
    $self->cache->set('default_config' => \%default_config);

    my $node = $opts->{'node'} || $default_config{'node'};
    my $host = $opts->{'host'} || $default_config{'host'};

    # Load the configuration file.
    if ($opts->{'config'} && -f $opts->{'config'}) {
        $opts->{'config'} = abs_path($opts->{'config'});
    }

    my $config_file;
    my @cf = (
        $ENV{'HOME'}."/.config/erlsvc/config-$node\@$host.yaml",
        $ENV{'HOME'}."/.config/erlsvc/config-$node.yaml",
        $ENV{'HOME'}.'/.config/erlsvc/config.yaml',
        "/etc/my_service/erlsvc-$node\@$host.yaml",
        "/etc/my_service/erlsvc-$node.yaml",
        '/etc/my_service/erlsvc.yaml',
        '/etc/default/'.progname()
    );
    unshift @cf, $opts->{'config'} if ($opts->{'config'});
    foreach my $cf (@cf) {
        if (-f $cf) {
            $config_file = $cf;
            last;
        }
    }
    my $yaml_errstr;
    if ($config_file) {
        local $@;

        my $yaml = eval {
            local $SIG{'__DIE__'};
            YAML::Tiny->read($config_file)
        };
        unless ($yaml) {
            $yaml_errstr = $@ || YAML::Tiny->errstr;
        }

        $self->cache->set('config'      => $yaml);
        $self->cache->set('config_file' => $config_file);
    }

    # Initialize the logging facility.
    my $logger = ErlSvc::Ctl::Log->new($self);
    $self->cache->set('logger' => $logger);

    $self->log->debug("APP",
        "erlsvc $VERSION -- CLI::Framework ".$CLI::Framework::VERSION."\n");

    if ($config_file) {
        $self->log->debug('APP', "Config file: $config_file\n");
        if ($yaml_errstr) {
            $self->log->error(
                "Problem:\n",
                "  Failed to read configuration file.\n",
                "  Configuration file is:\n",
                "    $config_file\n",
                "  YAML::Tiny reports:\n",
                "    $yaml_errstr\n");
            command_run_failure();
            return;
        }
    }

    my $erl_env = ErlSvc::Ctl::Erlang::Env->new($self);
    $self->cache->set('erl_env', $erl_env);

    $self->log->debug("APP", "<b>Ready to execute command</b>\n");
}

sub option_spec {
    return (
        [ 'config|C=s', 'specify configuration file path' ],
        [ 'user|u=s', 'run the Erlang node under the specified user', ],
        [ 'group|g=s', 'run the Erlang node under the specified group' ],
        [ 'node|n=s', 'specify Erlang node name' ],
        [ 'host|h=s', 'specify Erlang node host' ],
        [ 'cookie|c=s', 'specify Erlang cookie' ],
        [ 'release|r=s', 'specify release if the node is not running' ],
        [ 'releases-dir|d=s', 'specify the releases directory used by SASL' ],
        [],
        # Following options should be used carefully. They're intended
        # for development and debugging purposes.
        [ 'erlang|E=s', 'specify Erlang root directory' ],
        [ 'erllibs-dir|L=s@', 'set the path to Erlang libraries' ],
        [ 'mods-dir|M=s', 'set the path to erlsvc\'s modules' ],
        [ 'pipe-dir|P=s', 'set the path where runtime files are stored' ],
        [ 'log-dir|O=s', 'set the path where log files are stored' ],
        [ 'verbose|V:s@', 'print debugging messages for specified components' ],
        [],
        [ 'help', 'print usage and exit' ],
        [ 'version', 'print program version and exit' ]
    );
}

sub command_map {
        'help'            => 'ErlSvc::Ctl::Command::help',
        'mnesia'          => 'ErlSvc::Ctl::Command::mnesia',
        'release'         => 'ErlSvc::Ctl::Command::release',
        'reload'          => 'ErlSvc::Ctl::Command::reload',
        'restart'         => 'ErlSvc::Ctl::Command::restart',
        'shell'           => 'ErlSvc::Ctl::Command::shell',
        'start'           => 'ErlSvc::Ctl::Command::start',
        'status'          => 'ErlSvc::Ctl::Command::status',
        'stop'            => 'ErlSvc::Ctl::Command::stop',
        'target'          => 'ErlSvc::Ctl::Command::target',
        'version'         => 'ErlSvc::Ctl::Command::version',
}

sub command_alias {
        '--help'    => 'help',
        '--version' => 'version'
}

sub usage_text {
    my ($self) = @_;

    my $service_name = ErlSvc::Ctl::Service->name;
    my $usage = progname()." $VERSION - Start and control $service_name\n";
    $usage .= "\nUsage:\n".$self->get_default_usage();

    my $commands = $self->command_map_hashref();
    my @cmd_names = sort keys %$commands;

    my $max_len = 0;
    foreach my $cmd_name (@cmd_names) {
        my $len = length($cmd_name);
        $max_len = $len if ($len > $max_len);
    }

    $usage .= "\nAvailable commands:\n";
    foreach my $cmd_name (@cmd_names) {
        my $cmd = $commands->{$cmd_name};
        $usage .= sprintf("  %-${max_len}s - %s\n", $cmd_name,
            abstract($cmd));
    }

    return $usage;
}

sub handle_exception {
    my ($self, $e) = @_;

    if (ref($e) eq 'ErlSvc::Ctl::Exception::CmdRunException') {
        print $e->reason."\n" if $e->reason;
    } else {
        print $e->error."\n" if $e->error;
    }
}

sub uninit {
    my ($self) = @_;

    # Shutdown the target node.
    my $target = $self->node('no_init' => 1);
    $target->stop if (defined $target);

    # Shutdown the controller node.
    my $controller = $self->controller('no_init' => 1);
    $controller->stop if (defined $controller);
}

1;
