# -*- coding: utf-8 -*-
# pattern.py --- Time-stamp: <2010-07-13 15:06:57 Tuesday by julian>
# Copyright 2010 Julian Qian
# Author: julian@PDDES.cdc.veritas.com
# Version: $Id: pattern.py,v 0.0 2010/07/08 04:11:49 julian Exp $
# Keywords: 


import re

from media import Media

class Pattern:
    """
    """
    
    def __init__(self):
	self.MEDIAPATTERN = r'(http://[^"]+(gif|jpg))'
	self.SCRIPTPATTERN = r'<script\s+src="http://bulo\.\S+(http://[^&]+\.mp3).+</script>'
    
    def match(self, content):
	"""
	return matched url list
	"""
	urls = []
	for m in re.finditer(self.MEDIAPATTERN, content):
	    urls.append(m.group(0))
	for m in re.finditer(self.SCRIPTPATTERN, content):
	    urls.append(m.group(1))
	return urls

    # @staticmethod
    def matchMedia(self, obj):
	url = obj.group(0)
	return Media.path(url)

    def matchScript(self, obj):
	url = obj.group(1)
	mp3url = Media.path(url)
	return '<embed src="' + mp3url + '" loop=false autostart=false name="IMG_English" width="300" height="20"></embed>'
    
    def sub(self, content):
	"""
	replace content by patterns
	"""
	content = re.sub(self.MEDIAPATTERN, self.matchMedia, content)
	content = re.sub(self.SCRIPTPATTERN, self.matchScript, content)
	return content

def main():
    "pattern.py"
    import getopt, sys    

    pat = Pattern()
    from db import CrawlDB
    db = CrawlDB()

    try:
	opts, args = getopt.getopt(sys.argv[1:], "mr", ["match", "replace"])
    except getopt.GetoptError, e:
	print e
	sys.exit(2)
    if opts:
	for o, a in opts:
	    if o in ("-m", "--match"):	
		for page in db.getPages(True):
		    print pat.match(page["content"])
		    print "\n---\n"
	    if o in ("-r", "--replace"):
		for page in db.getPages(True):
		    print pat.sub(page["content"]).encode("utf-8")
		    print "\n---\n"
		    
if __name__ == "__main__":
    main()    
