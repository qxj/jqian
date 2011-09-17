#!/usr/bin/env python
# -*- coding: utf-8 -*-
# txt2mobi.py --- Time-stamp: <Julian Qian 2011-09-17 13:17:22>
# Copyright 2011 Julian Qian
# Author: junist@gmail.com
# Version: $Id: txt2mobi.py,v 0.0 2011/09/17 04:39:41 jqian Exp $

import sys, os, subprocess

def txt2mobi(filename):
    """
    """
    basename, ext = os.path.splitext(filename)
    tmpname = basename + ".html"
    tmp = open(tmpname, "w")
    tmp.write("<html><head><title>"+basename+"</title></head><body>")
    fp = open(filename, "r")
    for line in fp.readlines():
        if not line.isspace():
            tmp.write("<p>"+line+"</p>");
    tmp.write("</body></html>")
    tmp.close()

    cmd = 'kindlegen %s -o "%s.mobi" >> /var/tmp/txt2mobi.log' % (tmpname, basename)
    print cmd
    subprocess.call(cmd, shell=True)
    os.unlink(tmpname)

def main():
    "txt2mobi.py"
    txt2mobi(sys.argv[1])
    print "convert ok!"

if __name__ == "__main__":
    main()
