use strict;
use warnings;
use utf8;

my $share_dir = '../blib/lib/auto/share/dist/erlsvc';

my @erl = glob '*.erl';
my %beam = map {
    my $e = $_;
    my $b = $e;
    $b =~ s/\.erl$/.beam/o;
    $share_dir.'/'.$b => $e
} @erl;
my $beam_var = join(' ', keys %beam);

open(my $fh, '>', "Makefile") or die $!;

print $fh <<"EOF";
# Generated Makefile.

BEAM = $beam_var

all: \$(BEAM)

test:

clean:
	-rm -f \$(BEAM)

realclean: clean
	-rm -f Makefile.old Makefile

.PHONY: all test clean realclean
EOF

while (my ($beam, $erl) = each %beam) {
    print $fh <<"EOF";

$beam: $erl erlsvc.hrl
	\@install -d $share_dir
	erlc -o $share_dir $erl
EOF
}

close($fh);
