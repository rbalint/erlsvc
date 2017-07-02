# $Id: Makefile.PL 6244 2010-10-20 14:41:40Z jean.sebastien.pedron $

use strict;
use warnings;
use utf8;
use ExtUtils::MakeMaker;
use File::ShareDir::Install;

install_share 'share';

WriteMakefile(
    NAME => "erlsvc",
    VERSION_FROM => "lib/ErlSvc/Ctl.pm",
    EXE_FILES => ["script/erlsvc"],
);

package MY;
use File::ShareDir::Install qw(postamble);
