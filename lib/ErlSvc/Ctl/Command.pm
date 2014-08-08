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

package ErlSvc::Ctl::Command;
use base qw(CLI::Framework::Command ErlSvc::Ctl::Resources);

use strict;
use warnings;
use utf8;

use Getopt::Long::Descriptive;
use ErlSvc::Ctl::Usage qw(progname abstract);

sub short_desc () { abstract(shift); }
sub desc ()       { '';              }

sub usage_text {
    my $self = shift;

    my $progname = progname();

    my $cmd_name = ref($self) || $self;
    $cmd_name =~ s/.*::Command:://o;
    $cmd_name =~ s/::/ /go;

    my $usage = $progname.' '.$cmd_name.' - '.abstract($self)."\n";

    my $desc = $self->desc;
    $usage .= "\n".$desc if ($desc);

    my $format = $progname.' '.$cmd_name.' %o ...';
    my ($opts, $opts_usage);
    eval {
        ($opts, $opts_usage) = describe_options($format, $self->option_spec());
    };
    $usage .= "\nUsage:\n".$opts_usage->text();

    my @subcmd_names = $self->registered_subcommand_names();
    if (scalar @subcmd_names) {
        my $max_len = 0;
        foreach my $subcmd_name (@subcmd_names) {
            my $len = length($subcmd_name);
            $max_len = $len if ($len > $max_len);
        }

        $usage .= "\nAvailable subcommands:\n";
        foreach my $subcmd_name (@subcmd_names) {
            my $subcmd = $self->registered_subcommand_object($subcmd_name);
            $usage .= sprintf("  %-${max_len}s - %s\n", $subcmd_name,
                abstract($subcmd));
        }
    }

    return $usage;
}

1
