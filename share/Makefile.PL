use strict;
use warnings;
use utf8;
use ExtUtils::MakeMaker;

sub MY::postamble {
    return <<'EOF';
ERLS = $(wildcard *.erl)
BEAM_PATH = ../blib/lib/auto/share/dist/erlsvc
BEAMS = $(addprefix $(BEAM_PATH)/,$(ERLS:.erl=.beam))

all:: $(BEAMS)

$(BEAM_PATH)/%.beam: %.erl erlsvc.hrl
	@install -d $(BEAM_PATH)
	erlc -o $(BEAM_PATH) $<
EOF
}

WriteMakefile();
