# -*- coding: utf-8 -*-
# rsspub.py --- Time-stamp: <2010-07-29 11:12:26 Thursday by julian>
# Copyright 2010 Julian Qian
# Author: julian@PDDES.cdc.veritas.com
# Version: $Id: rsspub.py,v 0.0 2010/07/28 09:21:32 julian Exp $
# Keywords: 

from db import CrawlDB
from pattern import Pattern
from log import logger
import config
import getopt, sys, re

class RssPub(object):
    """
    Export all pages as rss pages.
    """
    # title, link, description, copyright, items
    rssframe = """<?xml version="1.0" encoding="UTF-8" ?>
<rss version="2.0">
<channel>
<title><![CDATA[%s]]></title> 
<link>%s</link> 
<description><![CDATA[%s]]></description> 
<language>zh-cn</language> 
<copyright><![CDATA[%s]]></copyright>
%s
</channel>
</rss>"""
    # link, title, author, category, pubdate, guid, description
    rssitem = """<item>
<link>%s</link>
<title><![CDATA[%s]]></title> 
<author>%s</author>
<category><![CDATA[%s]]></category>
<pubDate>%s</pubDate> 
<guid>%s</guid> 
<description>
<![CDATA[%s]]>
</description>
</item>
"""
    
    def export(self):
	"""
	"""
	logger.debug("Begin RSS Export:")
	db = CrawlDB()
	rep = Pattern()
	for pat in db.getPatterns():
	    pid = pat["pid"]
	    pattern = pat["pattern"]
	    description = pat["name"]
	    items = []
	    for page in db.getPages("where pid=%d limit 10" % pid):
		items.append(self.rssitem % (page["url"],
					     page["title"],
					     "",
					     pattern,
					     "",
					     page["url"],
					     rep.sub(page["content"])))
	    itemout = "\n".join(items)
	    output = self.rssframe % (pattern,
				      "http://hjbbs.com/bbs",
				      description,
				      "Learning English Tool",
				      itemout)
	    logger.debug("LET %d:\n%s\n" % (pid, output))
	    # write out
	    fp = open("%slet%d.xml" % (config.RSSDIR, pid), "w")
	    fp.write(output.encode('utf8'))
	    fp.close()
	logger.debug("End RSS Export.")

def main():
    "rsspub.py"
    rss = RssPub()
    rss.export()
    
if __name__ == "__main__":
    main()    
