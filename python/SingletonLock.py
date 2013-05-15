#!/usr/bin/env python
# -*- coding: utf-8; tab-width: 4; -*-
# @(#) SingletonLock.py  Time-stamp: <Julian Qian 2012-07-25 14:32:12>
# Author: Julian Qian <junist@gmail.com>
# Version: $Id: SingletonLock.py,v 0.1 2012-07-25 11:39:32 wbsvr Exp $
#

import fcntl, sys, os

class SingletonLock(object):
    """
    Usage:

    called before the script you wanna lock:

    lck = SingletonLock("/path/to/lock")
    if lck.locked():
        print "locked"
    else:
        print "unlocked"
    """
    def __init__(self, lockFile="/tmp/singleton.lck"):
        self.lock = lockFile
        self.fp = None

    def locked(self):
        try:
            self.fp = open(self.lock, "w")
            fcntl.flock(self.fp, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except IOError, e:
            self.fp.close()
            self.fp = None
            return True
        return False

    def __del__(self):
        if self.fp:
            fcntl.flock(self.fp, fcntl.LOCK_UN)
            self.fp.close()
            os.unlink(self.lock)

def main():
    import time
    print "begin"
    a = SingletonLock("abc.lck")
    if a.locked():
        print "hello locked"
    else:
        print "hello init lock"
        time.sleep(100)

if __name__ == "__main__":
    main()