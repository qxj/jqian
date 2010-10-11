# -*- coding: utf-8 -*-
# db.py --- Time-stamp: <2010-07-28 18:31:33 Wednesday by julian>
# Copyright 2010 Julian Qian
# Author: jqian@desktop
# Version: $Id: db.py,v 0.0 2010/06/23 10:54:21 jqian Exp $
# Keywords:

import sqlite3, os, getopt, time
import types
import config

class CrawlDB:
    """
    """
    def __init__(self, dbfile = config.DBPATH):
	is_init = False
	if os.path.isfile(dbfile) and os.path.exists(dbfile):
	    is_init = True
	else:
	    open(dbfile, "w").close()

	self.conn = sqlite3.connect(dbfile)
	if not is_init:			# init database
	    c = self.conn.cursor()
	    c.execute("create table urls (url text, title text, content text, pid integer, timestamp text, sent integer)")	    # url table
	    c.execute("create table patterns (pid integer primary key autoincrement, name text,pattern text,cid integer)")
	    c.execute("create table category (cid integer primary key autoincrement, name text)")
	    c.execute("create table emails (pid integer, email text)")
	    self.conn.commit()
	    c.close()

    def close(self):
	self.conn.close()

    def putUrl(self, pid, url, title = "", content = ""):
	c = self.conn.cursor()
	c.execute("insert into urls (url, title, content, pid, timestamp, sent) values (:url, :title, :content, :pid, :timestamp, 0)", {"url": url, "title": title, "content": content, "pid": pid, "timestamp": time.ctime()})
	self.conn.commit()
	c.close()

    def chkUrl(self, url):
	"""
	check whether one url has been retrieved
	"""
	c = self.conn.cursor()
	c.execute("select url from urls where url=:url", {"url": url})
	if c.fetchone():
	    return True
	return False

    def setUrl(self, url):
	c = self.conn.cursor()
	c.execute("update urls set sent=1 where url=:url", {"url": url})
	self.conn.commit()
	c.close()
	
    def unsetUrls(self):
	c = self.conn.cursor()
	c.execute("update urls set sent=0")
	self.conn.commit()
	c.close()

    def getPages(self, fetchall = False):
	"""
	get retrieved pages that have not been sent
	"""
	c = self.conn.cursor()
	if type(fetchall) is types.BooleanType:
	    if fetchall:
		c.execute("select url, title, content, pid from urls")
	    else:
		c.execute("select url, title, content, pid from urls where sent=0")
	elif type(fetchall) is types.IntType:
	    c.execute("select url, title, content, pid from urls limit %d" % fetchall)
	elif type(fetchall) is types.StringType:
	    c.execute("select url, title, content, pid from urls %s" % fetchall)
    
	ret = []
	for row in c:
	    ret.append({"url":     row[0],
			"title":   row[1],
			"content": row[2],
			"pid":     row[3]})
	c.close()
	return ret

    def getEmailByPid(self, pid):
	"""
	"""
	c = self.conn.cursor()
	c.execute("select email from emails where pid=:pid", {"pid": pid})
	return [row[0] for row in c]

    def getPatterns(self):
	"""
	"""
	c = self.conn.cursor()
	c.execute("select pid, pattern, name from patterns")
	patterns = []
	for row in c:
	    patterns.append({"pid": row[0],
			     "pattern": row[1],
			     "name": row[2]})
	return patterns


    @staticmethod
    def delDB():
	"""
	"""
	if os.path.exists(config.DBPATH):
	    os.unlink(config.DBPATH)
	
def main():
    "db.py"
    try:
	opts, args = getopt.getopt(sys.argv[1:], "du", ["delete", "unsent"])
    except getopt.GetoptError, e:
	print e
	sys.exit(2)
    if opts:
	for o, a in opts:
	    if o in ("-d", "--delete"):
		CrawlDB.delDB()
	    if o in ("-u", "--unsent"):
		db = CrawlDB()
		db.unsetUrls()
    else:
	db = CrawlDB()
	db.close()
    
if __name__ == "__main__":
    main()    
