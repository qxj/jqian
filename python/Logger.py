#!/usr/bin/env python
# -*- coding: utf-8; tab-width: 4; -*-
# @(#) Logger.py  Time-stamp: <Julian Qian 2010-05-15 11:24:39>
# Author: Julian Qian <junist@gmail.com>
# Version: $Id: Logger.py,v 0.1 2010-05-15 11:12:42 wbsvr Exp $
#

"""
Usage:

from Logger import getLogger

logger = getLogger("Log Name", "/path/to/logfile")
logger.info("blah blah")
"""

import sys, logging

def getLogger(logname = '',
              logfile = '',
              logformat = '%(asctime)s %(name)s [%(levelname)s] <%(filename)s:%(lineno)d> %(message)s'):
    """
    @arg log name
    @arg log file, if not empty, will append to a file
    @arg log format

    defaultly write log into stderr
    """
    logger = logging.getLogger(logname)
    logger.setLevel(logging.DEBUG)
    formatter = logging.Formatter(logformat)
    ## append to file
    if logfile:
        fh = logging.FileHandler(logfile)
        fh.setLevel(logging.DEBUG)
        fh.setFormatter(formatter)
        logger.addHandler(fh)
    ## append to sys.stderr
    ch = logging.StreamHandler()
    ch.setLevel(logging.DEBUG)
    ch.setFormatter(formatter)
    logger.addHandler(ch)
    return logger

def main():
    logger = getLogger("Test Logger")
    logger.info("hello logger~")

if __name__ == "__main__":
    main()