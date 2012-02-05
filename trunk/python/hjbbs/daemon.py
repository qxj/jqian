# -*- coding: utf-8 -*-
# daemon.py --- Time-stamp: <Qian Julian 2012-02-05 18:44:08>
# Copyright 2010 Julian Qian
# Author: jqian@desktop
# Version: $Id: daemon.py,v 0.0 2010/07/03 19:49:20 jqian Exp $
# Keywords:


import os               # Miscellaneous OS interfaces.
import sys              # System-specific parameters and functions.

UMASK = 0
WORKDIR = os.curdir
MAXFD = 1024

# The standard I/O file descriptors are redirected to /dev/null by default.
if (hasattr(os, "devnull")):
   REDIRECT_TO = os.devnull
else:
   REDIRECT_TO = "/dev/null"

def createDaemon():
   """
   Detach a process from the controlling terminal and run it in the
   background as a daemon.
   """

   try:
      pid = os.fork()
   except OSError, e:
      raise Exception, "%s [%d]" % (e.strerror, e.errno)

   if (pid == 0):   # The first child.
      os.setsid()
      # import signal           # Set handlers for asynchronous events.
      # signal.signal(signal.SIGHUP, signal.SIG_IGN)

      try:
         pid = os.fork()    # Fork a second child.
      except OSError, e:
         raise Exception, "%s [%d]" % (e.strerror, e.errno)

      if (pid == 0):    # The second child.
         os.chdir(WORKDIR)
         os.umask(UMASK)
      else:
         os._exit(0)    # Exit parent (the first child) of the second child.
   else:
      os._exit(0)   # Exit parent of the first child.

   import resource      # Resource usage information.
   maxfd = resource.getrlimit(resource.RLIMIT_NOFILE)[1]
   if (maxfd == resource.RLIM_INFINITY):
      maxfd = MAXFD

   # Iterate through and close all file descriptors.
   for fd in range(0, maxfd):
      try:
         os.close(fd)
      except OSError:   # ERROR, fd wasn't open to begin with (ignored)
         pass

   os.open(REDIRECT_TO, os.O_RDWR)  # standard input (0)
   os.dup2(0, 1)            # standard output (1)
   os.dup2(0, 2)            # standard error (2)

   return(0)

if __name__ == "__main__":

   retCode = createDaemon()

   procParams = """
   return code = %s
   process ID = %s
   parent process ID = %s
   process group ID = %s
   session ID = %s
   user ID = %s
   effective user ID = %s
   real group ID = %s
   effective group ID = %s
   """ % (retCode, os.getpid(), os.getppid(), os.getpgrp(), os.getsid(0),
   os.getuid(), os.geteuid(), os.getgid(), os.getegid())

   open("daemon.log", "w").write(procParams + "\n")

   sys.exit(retCode)
