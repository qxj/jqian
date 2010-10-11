# -*- coding: utf-8 -*-
# crawlweb.py --- Time-stamp: <2010-07-13 14:31:17 Tuesday by julian>
# Copyright 2010 Julian Qian
# Author: jqian@desktop
# Version: $Id: crawlweb.py,v 0.0 2010/06/13 04:58:17 jqian Exp $
# Keywords:

import re, codecs, urllib, urllib2, cookielib
import getopt, sys, os

from vcode import getCode
from media import Media
from db import CrawlDB
from pattern import Pattern
from log import logger
import config

import pdb


class CrawlPages:
    """
    """
    def __init__(self):
	"""
	"""
	self.db = CrawlDB()
	self.pat = Pattern()
	# self.patterns = config.PATTERNS
	self.titlere = re.compile(config.TITLEPATTERN)
	self.patterns = self.db.getPatterns() # unicode patterns

    def parseTitles(self):
	"""
	Fetch index page firstly, then search page content and figure out all
	links to be retrieved.
	@return list of dict(link, pattern-id) to be retrieved
	"""
	logger.info("root url: " + config.ROOTURL)
	sock = urllib2.urlopen(config.ROOTURL)
	lines = sock.readlines()
	sock.close()

	if config.DEBUG:
	    pdb.set_trace()

	logger.info("Index Content: %s" %
		    ("\n".join(lines)).decode("gbk"))
	    
	prelines = []
	for line in lines:
	    if len(line) > 10:		# trick, avoid useless matches
		for pat in self.patterns:
		    if line.find(pat["pattern"].encode("gbk")) != -1:
			prelines.append({"line": line,
					 "pid": pat["pid"]})
	
	logger.info("catched lines num: %d " % len(prelines))
		    
	prelinks = []
	for line in prelines:
	    mline = self.titlere.search(line["line"])
	    if mline:
		# check database
		newurl = "http://www.hjbbs.com/"+mline.group(1)
		if config.DEBUG:
		    pdb.set_trace()
		    
		if not self.db.chkUrl(newurl):
		    prelinks.append({"url": newurl,
				     "pid": line["pid"]})
	logger.info("links to be crawled num: %d " % len(prelinks))
	return prelinks
		
    def loginHjbbs(self):
	"""
	Login in hjbbs, and keep cookie.
	Call this function before crawl any other pages.
	@return A boolean value to indicate login or failed
	"""
	cookie_support= urllib2.HTTPCookieProcessor(cookielib.CookieJar())
	opener = urllib2.build_opener(cookie_support, urllib2.HTTPHandler)
	urllib2.install_opener(opener)

	tmpfile = "code.bmp"

	vcodebmp = urllib2.urlopen('http://hjbbs.com/GetCode.asp').read()
	vcodefile = open(tmpfile, 'wb')
	vcodefile.write(vcodebmp)
	vcodefile.close()
    
	vcodenum = getCode(tmpfile)
    
	postdata=urllib.urlencode({
		'username':config.USERNAME,
		'password':config.PASSWORD,
		'comeurl':'http://hjbbs.com/index.asp',
		'userhidden':'3',
		'submit':'登录',
		'CookieDate':3,
		'SecurityKey':vcodenum
		})
	postheaders = {"User-Agent":"Mozilla/5.0 (X11; U; Linux i686) Gecko/20071127 Firefox/2.0.0.11",
		       "Content-Type":"application/x-www-form-urlencoded",
		       "Referer":"http://hjbbs.com/login.asp",
		       "Connection":"keep-alive",
		       "Keep-Alive":115}
    
	req = urllib2.Request(
	    url = "http://hjbbs.com/login.asp?action=chk",
	    data = postdata,
	    headers = postheaders
	    )
	try:
	    res = urllib2.urlopen(req)
	except HTTPError, e:
	    logger.error("loginHjbbs http failed:" + e.reason)
	except URLError, e:
	    logger.error("loginHjbbs url failed:" + e.reason)
	else:
	    # fine
	    logger.info("Succeed to request loginHjbbs")
	    
	    html = res.read()
	    res.close()
	    if html.find("HTTP-EQUIV=REFRESH") != -1:
		logger.info("found refresh tag for loginHjbbs, ok")
		return True
	    else:
		logger.error("Not found REFRESH tag: " + html)
		
	logger.error("loginHjbbs some error? ")
	return False

    def catchTitle(self, content):
	pos1 = content.find("<title>")
	pos2 = content.find("</title>")
	if pos1 != -1 and pos2 != -1:
	    return content[pos1+7: pos2]
	return "no title"
    
    def catchTable(self, content, pattern = "id=\"bbs_hjd_1\"",
		   start_tag = "<table", end_tag = "</table>"):
	pos1 = pos2 = content.find(pattern)
	level = 1
	while True:		
	    pos3 = content.find(start_tag, pos2 + 1)
	    pos4 = content.find(end_tag, pos2 + 1)

	    if pos3 != -1 and pos4 != -1:
		if pos3 < pos4:
		    level = level + 1
		    pos2 = pos3
		else:
		    level = level - 1
		    pos2 = pos4
	    elif pos3 == -1 and pos4 != -1:
		level = level - 1
		pos2 = pos4
	    elif pos3 != -1 and pos4 == -1:
		level = level + 1
		pos2 = pos3
	    else:				# end tag not found
		break
	    if level == 0:
		break
	return start_tag + " " + content[pos1:pos2] + end_tag


    def crawlPage(self, url):
	"""
	crawl a page and transform internal links
	transform gb2312 to unicode
	@return transformed mail content and title
	"""
	try:
	    res = urllib2.urlopen(url)
	except HTTPError, e:
	    logger.error("crawlUrl failed: " + url + ": " + e.reason)
	except URLError, e:
	    logger.error("crawlUrl failed: " + url + ": " + e.reason)
	else:
	    logger.info("succeed to crawl url: " + url)

	    content = res.read()
	    title = self.catchTitle(content)
	    newcontent = self.catchTable(content)
	    self.medialinks = []
	    # newcontent = re.sub(r'(http://[^"]+(gif|jpg))', self.matchMedia, newcontent)
	    for url in self.pat.match(newcontent):
		self.crawlMedia(url)
		
	    return {"title": title.decode("gbk"),
		    "content": newcontent.decode("gbk")}
	return None
    
    def crawlMedia(self, link):
	"""
	crawl all media
	"""
	headers = {"User-Agent":"Mozilla/5.0 (X11; U; Linux i686) Gecko/20071127 Firefox/2.0.0.11",
		   "Referer":"http://hjbbs.com/index.asp",
		   "Connection":"keep-alive",
		   "Keep-Alive":115}
	try:
	    req = urllib2.Request(
		url = link,
		headers = headers
	    )
	    res = urllib2.urlopen(req)
	except Exception:
	    logger.error("crawlMedia failed: " + link)
	else:
	    media  = res.read()
	    fp = open(Media.dir(link), "wb")
	    fp.write(media)
	    fp.close()
	    logger.info("crawlMedia OK: " + link)
	
    @staticmethod
    def polishContent(content):
	"""
	@brief Polish content and return
	@param Content string
	@return Polished page content
	"""
	
    def crawlPages(self):
	"""
	Get all updated titles and its urls from the index page, and store
	them into sqlite db.
	"""
	logger.info("Begin to crawl...")
	if self.loginHjbbs():
	    for link in self.parseTitles():
		page = self.crawlPage(link["url"])
		# if config.DEBUG:
		#     pdb.set_trace()
		self.db.putUrl(link["pid"],
			       link["url"],
			       page["title"],
			       page["content"])
		logger.info("catched link: %s, title: %s" %
			       (link["url"], page["title"]))
	logger.info("End to crawl.")

def main():
    """
    Crawl web page and store them into database
    """
    crawl = CrawlPages()
    try:
	opts, args = getopt.getopt(sys.argv[1:], "cm", ["crawl", "mail"])
    except getopt.GetoptError, e:
	print e
	sys.exit(2)
    if opts:
	for o, a in opts:
	    if o in ("-c", "--crawl"):
		fp = open("/tmp/tmp-page.txt", "wb")
		if crawl.loginHjbbs():
		    for link in crawl.parseTitles():
			page = crawl.crawlPage(link)
			fp.write(link + "\r\n")
			fp.write(page["title"])
			fp.write(page["content"])
			print link
		else:
		    print "login failed"
	    elif o in ("-m", "--mail"):
		from mail import SendMail
		
		db    = CrawlDB()
		mail = SendMail()
		# search db
		pages = db.getPages()
		if pages:
		    for page in pages:
			if mail.sendMail(page["title"], page["content"]):
			    db.setUrl(page["url"])
		else:
		    print "no mail is sent"
		mail.close()
	    else:
		assert False, "unhandled option"
    else:
	# from time import strftime, gmtime
	# timefmt = strftime("%y-%m-%d", gmtime())
	# print "%s run crawl.crawlPages()" % (timefmt)
	
	crawl.crawlPages()

if __name__ == "__main__":
    main()    
