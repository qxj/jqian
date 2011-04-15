# -*- coding: utf-8 -*-
# media.py --- Time-stamp: <2010-07-08 12:09:25 Thursday by julian>
# Copyright 2010 Julian Qian
# Author: jqian@desktop
# Version: $Id: media.py,v 0.0 2010/06/23 10:44:26 jqian Exp $
# Keywords: 

from time import strftime, gmtime
from hashlib import md5

import config

class Media:
    """
    """
    @staticmethod
    def name(url):
	"""
	generate file name	
	"""
	# timefmt = strftime("%y-%m-%d", gmtime())
	suffix = url[url.rfind("."):]
	m = md5()
	m.update(url)
	# return timefmt + "_" + str(uniqname) + suffix
	return m.hexdigest() + suffix

    @staticmethod
    def path(url):
	"""
	URL path on web
	"""
	return config.MEDIAPATH + Media.name(url)

    @staticmethod
    def dir(url):
	"""
	File dir on disk
	"""
	return config.MEDIADIR + Media.name(url)


def main():
    "media.py"
    
if __name__ == "__main__":
    main()    
