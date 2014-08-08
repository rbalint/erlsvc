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

package ErlSvc::Ctl::Command::stop;
use base qw(ErlSvc::Ctl::Command);

=head1 NAME

ErlSvc::Ctl::Command::stop - Stop the service

=cut

use strict;
use warnings;
use utf8;

use ErlSvc::Ctl::Exceptions qw(:all);

use ErlSvc::Ctl::Service;
use ErlSvc::Ctl::Usage qw(abs_progname);

sub desc () {
    return <<'EOF';
This command stops the service.

If the service is not running, the command does nothing and returns
successfully.

Using this command on a remote node is supported but it's not possible
to restart it remotely!
EOF
}

sub option_spec {
    return (
        ['force|f',     'kill the service without a graceful stop at first'],
        ['timeout|t=i', 'kill the node if it\'s not down after TIMEOUT seconds']
    );
}

sub run {
    my ($self, $opts, @args) = @_;

    my $service = ErlSvc::Ctl::Service->new($self);
    my $service_name = ucfirst($service->name);

    my %params = ();

    if (exists $opts->{'timeout'}) {
        $params{'timeout'} = $opts->{'timeout'};
    }
    if ($opts->{'force'}) {
        $params{'force'} = 1;
    }

    my $ret = $service->stop(%params);
    if (!$ret) {
        # Failed to stop the service.
        command_run_failure();
        return;
    } elsif ($ret eq 2) {
        # Service already down.
        return "$service_name is not running\n";
    }

    return;
}

1;
