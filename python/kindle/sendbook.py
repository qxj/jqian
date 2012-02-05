#!/usr/bin/env python
# -*- coding: utf-8 -*-
# sendbook.py --- Time-stamp: <Qian Julian 2012-02-05 20:49:55>
# Copyright 2012 Qian Julian
# Author: junist@gmail.com
# Version: $Id: sendbook.py,v 0.0 2012/02/05 10:34:09 jqian Exp $

"""
Send book to xxx@free.kindle.com with your gmail account
"""

import smtplib, mimetypes, sys, os, pdb
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from email.mime.image import MIMEImage

# setting
KINDLEID = 'xxx@free.kindle.com'
USERNAME = 'user@gmail.com'
PASSWORD = 'password'

class SendBook(object):
    """
    """

    def __init__(self, kindleid, username, passwd):
        self.fromaddr = username
        self.toaddr = kindleid

        self.server = smtplib.SMTP('smtp.gmail.com', 587)
        self.server.starttls()
        self.server.login(username, passwd)

    def __enter__(self):
        print 'Begin to send book ...'
        return self

    def __exit__(self, type, value, traceback):
        self.server.quit()
        print 'End(SendBook).'
        return True

    def send(self, filename):
        # pdb.set_trace()
        msg = MIMEMultipart()

        msg['From'] = self.fromaddr
        msg['To'] = self.toaddr
        msg['Subject'] = "Convert %s" % os.path.basename(filename)

        ctype, encoding = mimetypes.guess_type(filename)
        if ctype is None or encoding is not None:
            ctype = 'application/octet-stream'
        maintype, subtype = ctype.split('/', 1)
        att = MIMEImage((lambda f: (f.read(), f.close()))(open(filename, 'rb'))[0], _subtype = subtype)
        att.add_header('Content-Disposition', 'attachment', filename = filename)
        msg.attach(att)

        self.server.sendmail(self.fromaddr, self.toaddr, msg.as_string())

def main():
    "sendbook.py"
    if len(sys.argv) == 1:
        print "usage:\n\t%s <file1> <file2> ...\n" % sys.argv[0]
        sys.exit(1)

    with SendBook(KINDLEID, USERNAME, PASSWORD) as s:
        for book in sys.argv[1:]:
            print "sending book %s" % book
            s.send(book)

if __name__ == "__main__":
    main()
