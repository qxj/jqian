# -*- coding: utf-8 -*-
#  table.py --- Time-stamp: <2010-03-31 15:38:26 Wednesday by jqian>
#  Copyright 2010 Julian Qian
#  Author: jqian@desktop
#  Version: $Id: menu.py,v 0.0 2010/03/29 07:15:31 jqian Exp $
#  Keywords: 

from google.appengine.ext import db
from google.appengine.api import users

"""
Menu management, you can add/edit/delete menu information for every restaurant.
"""

class ResTbl(db.Model):
    "Restaurant information"
    name = db.StringProperty(required=True)
    phone = db.PhoneNumberProperty(required=True)
    address = db.PostalAddressProperty()
    geo = db.GeoPtProperty()
    
class DishTbl(db.Model):
    "Dish properties"
    name = db.StringProperty(required=True)
    price = db.IntegerProperty(required=True)
    ratio = db.RatingProperty()
    res = db.ReferenceProperty(ResTbl)    

class OrderNumTbl(db.Model):
    """
    order number generator
    http://vrypan.net/log/2008/04/27/unique-integer-ids-in-google-datastore/
    """
    num = db.IntegerProperty()
    expired = db.BooleanProperty()
    sponsor = db.UserProperty()
    order_date = db.DateTimeProperty(auto_now_add=True)
    
class OrderTbl(db.Model):
    "Record order history"
    order_user = db.UserProperty(required=True)
    order_dish = db.ReferenceProperty(DishTbl)
    order_copy = db.IntegerProperty()
    order_num = db.ReferenceProperty(OrderNumTbl)
    order_date = db.DateTimeProperty(auto_now_add=True)
    
class CommentTbl(db.Model):
    "Store all comment for each dish."
    user = db.UserProperty(required=True)
    dish = db.ReferenceProperty(DishTbl)
    content = db.StringProperty(multiline=True)
    comment_date = db.DateTimeProperty(auto_now_add=True)


