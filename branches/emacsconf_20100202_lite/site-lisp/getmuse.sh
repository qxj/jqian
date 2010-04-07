#!/bin/sh
#  getmuse.sh --- Time-stamp: <2008-10-13 23:27:34 qianxjun>
#  Copyright 2008 Julian Qian
#  Author: qianxjun@IBM-L3KWG75
#  Version: $Id: getmuse.sh,v 0.0 2008/10/13 15:09:51 qianxjun Exp $
#  Keywords: 

# set -x

if [ -e "/usr/bin/git" ]
then
	git clone http://repo.or.cz/r/muse-el.git muse
else
	MUSETGZ=muse-latest.tar.gz
	if [ -f $MUSETGZ ]
	then
		rm $MUSETGZ
	fi
	wget http://mwolson.org/static/dist/$MUSETGZ
	tar zxf $MUSETGZ
	mv muse-latest/lisp muse
fi
