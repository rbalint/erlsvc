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

package ErlSvc::Ctl::Command::status;
use base qw(ErlSvc::Ctl::Command);

=head1 NAME

ErlSvc::Ctl::Command::status - Tell if the service is running

=cut

use strict;
use warnings;
use utf8;

use ErlSvc::Ctl::Exceptions qw(:all);

use ErlSvc::Ctl::Service;

sub desc () {
    return <<'EOF';
This command tells if the service is up and running.
EOF
}

sub option_spec {
    return (
        ['batch|b', 'exit with 0 if the service is running, otherwise 1; nothing printed'],
        ['short|s', 'just print "YES" or "NO" instead of a full sentence']
    );
}

sub run {
    my ($self, $opts, @args) = @_;

    my $service = ErlSvc::Ctl::Service->new($self);

    my $is_running = $service->is_running;

    if ($opts->{'batch'}) {
        command_run_failure() unless ($is_running);
        return;
    } elsif ($opts->{'short'}) {
        # The -s option is used by My service's Debian package and, because
        # the maintainer scripts use sh -e, we can't exit with 1 or
        # those scripts would exit too.
        return ($is_running) ? 'YES' : 'NO';
    } else {
        my $name = ucfirst($service->name);
        if ($is_running) {
            return "$name is up and running\n";
        } else {
            command_run_failure('reason' => "$name is NOT running");
            return;
        }
    }
}

1;
