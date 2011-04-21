# -*- coding: utf-8 -*-
# books.py --- Time-stamp: <Julian Qian 2011-04-19 15:29:13>
# Copyright 2011 Julian Qian
# Author: junist@gmail.com
# Version: $Id: books.py,v 0.0 2011/04/15 07:04:48 jqian Exp $
# Keywords:

import os

from google.appengine.ext import webapp
from google.appengine.ext.webapp import util
from google.appengine.ext.webapp import template
from google.appengine.ext.webapp.util import run_wsgi_app

class MainHandler(webapp.RequestHandler):
    "Entry Page"
    def get(self):
        template_values = {
            "books" : [{"name": "C++ Notes", "path": "notes-cpp/index.html"},
                       {"name": "Linux C编程一站式学习", "path": "akabook/index.html"},
                       {"name": "Perl学习手记", "path": "perl-toc/TOC.html"},]
            }
	path = os.path.join(os.path.dirname(__file__), 'tpl/books.html')

	self.response.out.write(template.render(path, template_values))

def main():
    application = webapp.WSGIApplication([("/", MainHandler)],
					 debug=True)
    util.run_wsgi_app(application)

if __name__ == "__main__":
    main()
