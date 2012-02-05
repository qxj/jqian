#!/usr/bin/env python
# -*- coding: utf-8 -*-
# sendbook.py --- Time-stamp: <Qian Julian 2012-02-05 22:10:38>
# Copyright 2012 Qian Julian
# Author: junist@gmail.com
# Version: $Id: sendbook.py,v 0.0 2012/02/05 10:34:09 jqian Exp $

"""
Send book to xxx@free.kindle.com with your gmail account
"""

import mimetypes, sys, os, pdb
from smtplib import SMTP, quotedata, CRLF, SMTPDataError
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from email.mime.image import MIMEImage

# setting
KINDLEID = 'xxx@free.kindle.com'
USERNAME = 'user@gmail.com'
PASSWORD = 'password'

class ExtendedSMTP(SMTP):
    def data(self, msg):
        """
        This is a modified copy of smtplib.SMTP.data()

        Sending data in chunks and calling self.callback
        to keep track of progress,
        """
        self.putcmd("data")
        (code,repl)=self.getreply()
        if self.debuglevel >0 : print>>sys.stderr, "data:", (code,repl)
        if code != 354:
            raise SMTPDataError(code,repl)
        else:
            q = quotedata(msg)
            if q[-2:] != CRLF:
                q = q + CRLF
            q = q + "." + CRLF

            # begin modified send code
            chunk_size = 10240
            bytes_sent = 0

            while bytes_sent != len(q):
                chunk = q[bytes_sent:bytes_sent+chunk_size]
                self.send(chunk)
                bytes_sent += len(chunk)
                if hasattr(self, "callback"):
                    self.callback(bytes_sent, len(q))
            # end modified send code

            (code, msg) = self.getreply()
            if self.debuglevel > 0 : print>>sys.stderr, "data:", (code,msg)
            return (code,msg)

def callback(progress, total):
    print "[%d%%] %d sent of %d bytes" % (progress*100/total, progress, total)

class SendBook(object):
    """
    """

    def __init__(self, kindleid, username, passwd):
        self.fromaddr = username
        self.toaddr = kindleid

        self.smtp = ExtendedSMTP('smtp.gmail.com', 587)
        self.smtp.callback = callback
        self.smtp.starttls()
        self.smtp.login(username, passwd)

    def __enter__(self):
        print 'Begin to send book ...'
        return self

    def __exit__(self, type, value, traceback):
        self.smtp.quit()
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

        self.smtp.sendmail(self.fromaddr, self.toaddr, msg.as_string())

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
