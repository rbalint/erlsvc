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

package ErlSvc::Ctl::Command::start;
use base qw(ErlSvc::Ctl::Command);

=head1 NAME

ErlSvc::Ctl::Command::start - Start the service

=cut

use strict;
use warnings;
use utf8;

use ErlSvc::Ctl::Exceptions qw(:all);

use ErlSvc::Ctl::Service;
use ErlSvc::Ctl::Usage qw(abs_progname);

sub desc () {
    return <<'EOF';
This command starts the service.

If the service is already running, the command does nothing and returns
successfully.

Using this command on a remote node is not possible.
EOF
}

sub option_spec {
    return (
        ['disable-heart|H',  'disable node monitoring with heart(1)'],
        ['foreground|F',     'start the service in an Erlang shell'],
        ['load-only|L',      'load the service but do not start it'],
        ['extra-flags|E=s@', 'add extra flags to the node start command line']
    );
}

sub run {
    my ($self, $opts, @args) = @_;

    my $service = ErlSvc::Ctl::Service->new($self);
    my $service_name = ucfirst($service->name);

    my %params = ();

    # Heart and SASL command to restart the node.
    $params{'restart_cmd'} = join(' ',
        abs_progname(),
        $self->opts_to_list,
        'restart',
        $self->opts_to_list($opts)
    );

    if (!$opts->{'disable_heart'}) {
        $params{'heart'} = 1;
    }
    if ($opts->{'foreground'}) {
        $params{'embedded'} = 0;
    }
    if ($opts->{'load_only'}) {
        $params{'embedded'}  = 0;
        $params{'load_only'} = 1;
    }
    if ($opts->{'extra_flags'}) {
        $params{'extra_flags'} = $opts->{'extra_flags'};
    }

    my $ret = $service->start(%params);
    if (!$ret) {
        # Failed to start the service.
        command_run_failure();
        return;
    } elsif ($ret eq 2) {
        # Service already up.
        return "$service_name is already running\n";
    }

    return;
}

1;
