-*- org -*-

* requirement
** python
version 2.6+

** sqlite
version 3.0+

** local mail server
eg: postfix or exim, etc.

* crawl process
** Run
directly run ./crawlweb.py to crawl web pages

** Cron
Or put this script into crontab

$ crontab -e
# run at 2:00am every midnight
0 2 * * * /path/to/crawlweb.py

* send mail process
** Run
directly run ./mail.py to send batch mail

** Cron
Or put this script into crontab

$ crontab -e
# run at 8:00 am every morning
0 8 * * * /path/to/mail.py

* run web server
** run
*** simle way
directly run ./hjbbs.py to start a simple web server, users can directly visit
this page from http://your.ip.address:8080.

*** Better performance
run lighttpd or apache with fast_cgi or scgi.

** web pages
*** subscribe page
visit http://address/

*** search pattern management page
visit http://address/pat
