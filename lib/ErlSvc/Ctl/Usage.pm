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

package ErlSvc::Ctl::Usage;

use strict;
use warnings;
use utf8;

require Exporter;

our @ISA = qw(Exporter);
our @EXPORT_OK = qw(abs_progname progname abstract);

use File::Basename;
use Cwd qw(abs_path);

sub abs_progname () {
    return abs_path($0);
}

sub progname () {
    return basename($0);
}

sub abstract ($) {
    my ($class) = @_;
    $class = ref $class if ref $class;

    my $result;

    (my $pm_file = $class) =~ s!::!/!g;
    $pm_file .= '.pm';
    require $pm_file unless (exists $INC{$pm_file});
    $pm_file = $INC{$pm_file};
    return "(failed to require '$class')" unless ($pm_file);

    open my $fh, "<", $pm_file or return "(unknown)";

    local $/ = "\n";
    my $inpod;

    while (local $_ = <$fh>) {
        $inpod = /^=cut/ ? !$inpod : $inpod || /^=(?!cut)/;

        next unless $inpod;
        chomp;
        next unless /^(?:$class\s-\s)(.*)/;
        $result = $1;
        last;
    }
    return $result || "(unknown)";
}

1;
