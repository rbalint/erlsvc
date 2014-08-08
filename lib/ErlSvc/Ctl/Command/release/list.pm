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

package ErlSvc::Ctl::Command::release::list;
use base qw(ErlSvc::Ctl::Command::release);

=head1 NAME

ErlSvc::Ctl::Command::release::list - List available releases

=cut

use strict;
use warnings;
use utf8;

use ErlSvc::Ctl::Exceptions qw(:all);
use ErlSvc::Ctl::Release;

sub desc () {
    return <<'EOF';
This command lists available releases and their respective state.
EOF
}

sub run {
    my ($self, $opts, @args) = @_;

    my $release = ErlSvc::Ctl::Release->new($self);

    my %rels = $release->list;

    if (%rels) {
        my $max_len = length('Release');
        foreach my $rel (keys %rels) {
            my $len = length($rel);
            $max_len = ($len > $max_len) ? $len : $max_len;
        }

        my $output = '';

        $output .= sprintf("\e[1m%-${max_len}s   %s\e[0m\n",
            'Release', 'State');
        my @relnames = sort { $a cmp $b } keys %rels;
        foreach my $rel ( @relnames) {
            $output .= sprintf("%-${max_len}s   %s\n",
                $rel, $rels{$rel}->{'state'});
        }
        return $output;
    } else {
        command_run_failure();
    }
}

1;
