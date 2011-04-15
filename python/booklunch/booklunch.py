# -*- coding: utf-8 -*-
#  booklunch.py --- Time-stamp: <2010-04-01 01:11:48 by Julian>
#  Copyright 2010 Julian Qian
#  Author: jqian@desktop
#  Version: $Id: booklunch.py,v 0.0 2010/03/29 04:17:00 jqian Exp $
#  Keywords: 

import cgi
import os

from google.appengine.api import users
from google.appengine.ext import webapp
from google.appengine.ext.webapp import util
from google.appengine.ext.webapp.util import run_wsgi_app
from google.appengine.ext import db
from google.appengine.ext.webapp import template

from urllib import unquote

from table import *

"""
One web application to help us book lunch easily.

Features:
 - one global board which can be visited easily
 - all restaurant's menu and meat prices
 - bind to our gmail account
 - record our lunch histroy and print out expense sheets
 - mail us report form monthly
"""

class MainHandler(webapp.RequestHandler):
    "Entry Page"
    def get(self):

	user = users.get_current_user()
	if user:
	    user_nickname = user.nickname()
	    url = users.create_logout_url(self.request.uri)
	    url_title = 'Logout'
	else:
	    user_nickname = ""
	    url = users.create_login_url(self.request.uri)
	    url_title = 'Login'

	rests = db.Query(ResTbl)
	first_rest_name = "N/A"
	if rests.get():
	    first_rest_name = rests.get().name
	
	ordernum = db.Query(OrderNumTbl)

	template_values = {
	    "rest_list": rests.order("name"),
	    "order_list": ordernum.order("-order_date").fetch(10),
	    "user_nickname": user_nickname,
	    "first_rest_name": first_rest_name,
	    "url": url,
	    "url_title": url_title,
	    }

	path = os.path.join(os.path.dirname(__file__), 'tpl/booklunch.html')
	self.response.out.write(template.render(path, template_values))

class MenuHandler(webapp.RequestHandler):
    "List menu and order them"
    def get(self, rname):
	# decode url
	rname = unicode(unquote(rname), "utf-8")
	# fetch restaurant name
	menu = db.Query(DishTbl)
	rest = ResTbl.get_by_key_name(rname)
	if rname:
	    menu.filter("res =", rest)

	template_values = {
	    "menu_list": menu.order("-price"),
	    "rest": rest,
	    "rest_name": rname
	    }

	path = os.path.join(os.path.dirname(__file__), 'tpl/menu.html')
	self.response.out.write(template.render(path, template_values))

    def post(self, rname):
	"order menu"
	# valid user
	user = users.get_current_user()
	if not user:
	    users.create_login_url(self.request.uri)
	    
	order_num = self.request.get("order_num")
	if order_num:
	    orderobj = OrderNumTbl().get_by_key_name(order_num)
	else:
	    # new order
	    prevobj = db.Query(OrderNumTbl).order("-order_date").get()
	    if prevobj:
		num = int(prevobj.num)
	    else:
		num = 0 # it is first order
	    orderobj = OrderNumTbl(key_name = str(num + 1),
				   num = num + 1,
				   sponsor = user,
				   expired = False)
	    orderobj.put()
	
	order_menu = self.request.get("order_menu")
	if not order_menu:
	    self.response.out.write("Please select your dishes.")
	    return
	
	for menu in order_menu.split("\n"):
	    obj = OrderTbl(order_user = user )
	    obj.order_dish = DishTbl.get_by_key_name(menu)
	    # TODO:
	    obj.order_copy = 1
	    obj.order_num = orderobj
	    obj.put()
	    
	self.response.out.write("Ordered")

class OrderHandler(webapp.RequestHandler):
    "show order detail"
    def post(self):
	onum = self.request.get("order_num")
	order_num = OrderNumTbl.get_by_key_name(onum)
        # fetch order information
	order_list = OrderTbl.gql("WHERE order_num = :1", order_num)

	template_values = {
	    "order_list" : order_list
	    }
	path = os.path.join(os.path.dirname(__file__), 'tpl/order.html')
	self.response.out.write(template.render(path, template_values))    
def main():
    application = webapp.WSGIApplication([("/", MainHandler),
					  (r"/menu/(.*)", MenuHandler),
					  ("/order/", OrderHandler)],
					 debug=True)
    util.run_wsgi_app(application)
    
if __name__ == "__main__":
    main()
    
