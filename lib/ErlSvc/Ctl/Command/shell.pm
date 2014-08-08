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

package ErlSvc::Ctl::Command::shell;
use base qw(ErlSvc::Ctl::Command);

=head1 NAME

ErlSvc::Ctl::Command::shell - Open a shell connected to a running service

=cut

use strict;
use warnings;
use utf8;

use ErlSvc::Ctl::Exceptions qw(:all);

use ErlSvc::Ctl::Service;
use ErlSvc::Ctl::Usage qw(abs_progname);

sub desc () {
    return <<'EOF';
This command opens a shell on a running service.

Two modes are available:
    o  Based on to_erl(1), this shell provides completion and history.
       Furthermore, everything is logged in erlang.log.*. However, only one
       shell can be opened at a time and the service must be local.
    o  The remote shell allows one to connect to a remote service but the
       commands history isn't kept after the shell is closed and completion
       isn't available.

If the node is local, this command tries the first mode and, if it
fails, falls back on the second mode. If the node is remote, this
command tries only the second mode.

The mode can be forced using command options.
EOF
}

sub option_spec {
    return (
        ['to-erl|t', 'open a shell using to_erl(1)'],
        ['remsh|r',  'open a shell using erl -remsh']
    );
}

sub run {
    my ($self, $opts, @args) = @_;

    my $service = ErlSvc::Ctl::Service->new($self);

    my %params = ();

    if ($opts->{'to_erl'}) {
        $params{'mode'} = 'to_erl';
    }
    if ($opts->{'remsh'}) {
        $params{'mode'} = 'remsh';
    }

    my $ret = $service->shell(%params);
    if (!$ret) {
        # Failed to shell the service.
        command_run_failure();
        return;
    }

    return;
}

1;
