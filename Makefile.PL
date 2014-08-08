# $Id: Makefile.PL 6244 2010-10-20 14:41:40Z jean.sebastien.pedron $

use strict;
use warnings;
use utf8;

use inc::Module::Install;

# Define metadata
name           'erlsvc';
version_from   'lib/ErlSvc/Ctl.pm';
author         'Jean-Sébastien Pédron <jean-sebastien@yakaz.com>';
license        'bsd';

requires       'Class::Inspector' => 0;
requires       'Exception::Class' => 0;
requires       'Exception::Class::TryCatch' => 0;
requires       'Getopt::Long::Descriptive' => 0;
requires       'YAML::Tiny' => 0;
requires       'File::ShareDir' => 0;

# Specific dependencies
install_script 'script/erlsvc';
install_share;

WriteAll;

sub MY::postamble {
	return if $Module::Install::VERSION > 0.76;

	my $cmd = "cd share && perl Makefile.PL";
	print "$cmd\n";
	system($cmd);

	return <<'EOF';

subdirs ::
	$(NOECHO) cd share && $(MAKE) $(USEMAKEFILE) $(FIRST_MAKEFILE) all $(PASSTHRU)

clean ::
	$(ABSPERLRUN)  -e 'chdir '\''share'\'';  system '\''$(MAKE) clean'\'' if -f '\''$(FIRST_MAKEFILE)'\'';' --

realclean ::
	- $(ABSPERLRUN)  -e 'chdir '\''share'\'';  system '\''$(MAKE) $(USEMAKEFILE) $(MAKEFILE_OLD) realclean'\'' if -f '\''$(MAKEFILE_OLD)'\'';' --
	- $(ABSPERLRUN)  -e 'chdir '\''share'\'';  system '\''$(MAKE) $(USEMAKEFILE) $(FIRST_MAKEFILE) realclean'\'' if -f '\''$(FIRST_MAKEFILE)'\'';' --

EOF
}
