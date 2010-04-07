#!/bin/sh

cvs -d:pserver:anonymous@cedet.cvs.sourceforge.net:/cvsroot/cedet login
# If prompt password, just enter to skip.
cvs -z3 -d:pserver:anonymous@cedet.cvs.sourceforge.net:/cvsroot/cedet co -P cedet

# After co cedet, please run `emacs -Q -l cedet-build.el -f cedet-build`.
# You can read cedet-build.el for more details.

