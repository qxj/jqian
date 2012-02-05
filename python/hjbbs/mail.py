# -*- coding: utf-8 -*-
# mail.py --- Time-stamp: <Qian Julian 2012-02-05 18:36:35>
# Copyright 2010 Julian Qian
# Author: jqian@desktop
# Version: $Id: mail.py,v 0.0 2010/06/23 10:47:51 jqian Exp $
# Keywords:

from email.MIMEText import MIMEText
from smtplib import SMTP
from db import CrawlDB
from pattern import Pattern
from log import logger
import getopt, sys, re
import config

class SendMail:
    def __init__(self):
        """
        """
        self.prefix = config.MAILPREFIX
        self.mailfrom = config.MAILFROM

        try:
            self.server = SMTP(config.MAILHOST)
            self.server.ehlo()
        except IOError,e:
            print e
        except MissingSectionHeaderError,e:
            print e
        except UnicodeDecodeError,e:
            print e

    def mailAddrs(self):
        fp = open(r'./address.txt','r')
        text = fp.read()
        fp.close()

        regex = ur'[a-zA-Z0-9_\-]+@[a-zA-Z0-9_\-]+\.[a-zA-Z0-9_\-.]+'
        addrs = re.findall(regex, text)
        return addrs

    def addrTo(self, addrlist):
        return ", ".join(addrlist)

    def sendMail(self, to, title, content):
        """
        Generate mail's html body,
        replace patterns before sendmail
        @param a list of email
        @title title string
        @content content string
        """
        html = MIMEText(content.encode("utf8"),
                        _subtype='html',
                        _charset='utf-8')
        html["Subject"] = "[%s] %s" % (self.prefix, title)
        html["From"] = self.mailfrom
        html["To"] = self.addrTo(to)

        try:
            self.server.sendmail(self.mailfrom, to, html.as_string())
        except SMTPRecipientsRefused, e:
            print e
        except Exception:
            print "send mail failed"
        else:
            return True
        return False

    def close(self):
        self.server.quit()


def main():
    "mail.py"
    mail  = SendMail()
    try:
        opts, args = getopt.getopt(sys.argv[1:], "t", ["test"])
    except getopt.GetoptError, e:
        print e
        sys.exit(2)
    if opts:
        for o, a in opts:
            if o in ("-t", "--test"):
                to = ["junist@gmail.com",
                      "Julian_Qian@symantec.com",
                      "kxmz@qq.com"]
                title = u"test title here"
                content = """
Begin of test<br/>
<embed type="application/x-shockwave-flash" src="http://www.odeo.com/flash/audio_player_standard_gray.swf" width="400" height="52" allowScriptAccess="always" wmode="transparent" flashvars="external_url=http://172.29.7.127:8000/static/media/35130a760bfc7e5054526ce94c17004f.mp3" />
Another
 <embed src="http://172.29.7.127:8000/static/media/35130a760bfc7e5054526ce94c17004f.mp3" loop=false autostart=false name="IMG_English" width="300" height="20" /><br/>End of test
"""
                mail.sendMail(to, title, content.decode("utf-8"))
    else:
        # send web pages to mail Subscriber
        logger.info("Begin to send email ...")

        pat = Pattern()
        db = CrawlDB()
        pages = db.getPages()
        if pages:
            logger.debug("Fetched %s pages." % (len(pages)))
            for page in pages:
                addrlist = db.getEmailByPid(page["pid"])
                if addrlist:
                    logger.debug("send mail to %s persons..." % (len(addrlist)))
                    content = pat.sub(page["content"])

                    if mail.sendMail(addrlist,
                                     page["title"],
                                     content):
                        db.setUrl(page["url"])
                        logger.info("Page [%s] is sent to %s\n\n%s\n\n" %
                                    (page["title"], ",".join(addrlist),
                                     content))
        else:
            logger.info("no mail is sent")
        logger.info("End to send email.")

    mail.close()

if __name__ == "__main__":
    main()
