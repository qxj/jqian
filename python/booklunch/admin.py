# -*- coding: utf-8 -*-
#  admin.py --- Time-stamp: <2010-04-01 10:04:30 Thursday by jqian>
#  Copyright 2010 Julian Qian
#  Author: Julian@JULIAN-PC
#  Version: $Id: admin.py,v 0.0 2010/03/29 13:59:27 Julian Exp $
#  Keywords: 

import cgi
import os

from google.appengine.ext import webapp
from google.appengine.ext.webapp import util
from google.appengine.ext.webapp.util import run_wsgi_app
from google.appengine.ext.webapp import template

from urllib import unquote

from table import *

class AdminHandler(webapp.RequestHandler):
    """
    """
    def get(self):
	template_values = {
	    }

	path = os.path.join(os.path.dirname(__file__), 'tpl/adm/admin.html')
	self.response.out.write(template.render(path, template_values))

class RestHandler(webapp.RequestHandler):
    """
    Restaurant information, list their menus
    """
    def get(self):
	rests = db.Query(ResTbl)
	rests = ResTbl.all()
	template_values = {
	    "rests": rests.order("name")
	    }

	path = os.path.join(os.path.dirname(__file__), 'tpl/adm/rest.html')
	self.response.out.write(template.render(path, template_values))

class RestAddHandler(webapp.RequestHandler):
    "add restaurant handler"
    def get(self):
	"add restaurant view"
	template_values	 = {}
	path = os.path.join(os.path.dirname(__file__), 'tpl/adm/rest-add.html')
	self.response.out.write(template.render(path, template_values))
        
    def post(self):
	"add restaurant information"
	rest = ResTbl(key_name = self.request.get("name"),
		      name = self.request.get("name"),
		      phone = self.request.get("phone"),
		      address = self.request.get("address"))
	rest.put()
	self.response.out.write("Add restaurant OK.")
	
class MenuHandler(webapp.RequestHandler):
    "List menu of one restaurant"
    def get(self, rname):
	# fetch restaurant name
	rname = unicode(unquote(rname), "utf-8")
	
	menu = db.Query(DishTbl)
	menu = DishTbl.all()
	if rname:
	    rest = ResTbl.get_by_key_name(rname)
	    menu.filter("res =", rest)
	else:
	    self.response.out.write("ErRor: No restaurant name?")
	    return
	
	template_values = {
	    "rest": rest,
	    "menu_list": menu.order("-price"),
	    "rest_name": rname
	    }

	path = os.path.join(os.path.dirname(__file__), 'tpl/adm/menu.html')
	self.response.out.write(template.render(path, template_values))	

    def post(self, rname):
	"add new dish menu to restaurant"
	rest_name = self.request.get("rest")
	# not necessary when post data
	# rest_name = unicode(unquote(rest_name), "utf-8") 
	dish_name = self.request.get("name")
	# dish_name = unicode(unquote(dish_name), "utf-8")
	dish_price = self.request.get("price")
	
	menu = DishTbl(key_name = dish_name,
		       name = dish_name,
		       price = int(dish_price),
		       res = ResTbl.get_by_key_name(rest_name))
	menu.put()
	self.response.out.write("Add new menu OK")

class HistoryHandler(webapp.RequestHandler):
    "Ordering history"
    def get(self):
	user = users.get_current_user()
	order_list = db.Query(OrderTbl)
	order_list = order_list.filter("order_user =", user)
	template_values = {
	    "order_list" : order_list.order("-order_date")
	    }

	path = os.path.join(os.path.dirname(__file__), 'tpl/adm/history.html')
	self.response.out.write(template.render(path, template_values))
	

def main():
    application = webapp.WSGIApplication([("/admin/", AdminHandler),
					  ("/admin/rest", RestHandler),
					  ("/admin/rest/add", RestAddHandler),
					  ("/admin/history", HistoryHandler),
					  (r"/admin/menu/(.*)", MenuHandler)],
					 debug=True)
    util.run_wsgi_app(application)
    
if __name__ == "__main__":
    main()

