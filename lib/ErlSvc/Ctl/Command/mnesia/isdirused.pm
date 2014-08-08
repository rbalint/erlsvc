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

package ErlSvc::Ctl::Command::mnesia::isdirused;
use base qw(ErlSvc::Ctl::Command::mnesia);

=head1 NAME

ErlSvc::Ctl::Command::mnesia::isdirused - Tell if Mnesia directory is in use

=cut

use strict;
use warnings;
use utf8;

use ErlSvc::Ctl::Exceptions qw(:all);
use ErlSvc::Ctl::Mnesia;

sub desc () {
    return <<'EOF';
This command tells if Mnesia directory is in use.

While using this command on a running and/or remote node is possible,
options modifying the release or the Mnesia directory have no effects.
EOF
}

sub option_spec {
    return (
        ['short|s', 'just print "YES" or "NO" instead of a full sentence']
    );
}

sub run {
    my ($self, $opts, @args) = @_;

    my $mnesia = ErlSvc::Ctl::Mnesia->new($self);

    my %params = ();

    my $directory = $self->mnesia_dir;
    if ($directory) {
        $params{'directory'} = $directory;
    } else {
        $params{'release'} = $self->opts('release');
    }

    my $is_used = $mnesia->is_directory_used(%params);
    unless (defined $is_used) {
        command_run_failure();
    }

    if ($opts->{'short'}) {
        return ($is_used ? 'YES' : 'NO')."\n";
    } else {
        $directory = $mnesia->directory unless ($directory);
        return "The directory '$directory' is ".($is_used ? '' : 'NOT ').
          "used by Mnesia\n";
    }
}

1;
