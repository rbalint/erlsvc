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

package ErlSvc::Ctl::Command::release::upgrade;
use base qw(ErlSvc::Ctl::Command::release);

=head1 NAME

ErlSvc::Ctl::Command::release::upgrade - Upgrade to specified release

=cut

use strict;
use warnings;
use utf8;

use ErlSvc::Ctl::Exceptions qw(:all);

sub desc () {
    return <<'EOF';
This command upgrades the current release to a specified release.

Using this command on a remote node is not possible.
EOF
}

sub option_spec {
    return (
        ['extra-flags|E=s@', 'add extra flags to the node start command line']
    );
}

sub run {
    my ($self, $opts, @args) = @_;

    my $release = ErlSvc::Ctl::Release->new($self);

    my $current = $release->current;
    my $target  = $args[0];

    my %params = ();

    if ($opts->{'extra_flags'}) {
        $params{'extra_flags'} = $opts->{'extra_flags'};
    }

    my $ret = $release->upgrade($target, %params);

    unless ($ret) {
        # Failed to upgrade the release.
        command_run_failure();
    }

    if ($ret == 2) {
        return "Service is already using release '$target'\n";
    } else {
        return "Service upgraded from release '$current' to '$target'\n";
    }
}

1;
