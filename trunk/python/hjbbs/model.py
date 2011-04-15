# -*- coding: utf-8 -*-
# model.py --- Time-stamp: <2010-07-09 10:46:47 Friday by julian>
# Copyright 2010 Julian Qian
# Author: jqian@desktop
# Version: $Id: model.py,v 0.0 2010/06/23 09:50:24 jqian Exp $
# Keywords: 

import web

import config

db = web.database(dbn='sqlite', db=config.DBPATH )

def get_pattern():
    return db.select('patterns', order='cid')

def get_category():
    return db.select('category', order='sort')

def new_pattern(name, pattern, cid):
    db.insert('patterns', name=name, pattern=pattern, cid=cid)

def del_pattern(pid):
    db.delete('patterns', where="pid=$pid", vars=locals())

def get_pid_by_email(email):
    email = email.strip().lower()
    return db.select('emails', where="email=$email", vars=locals())

def new_pat2mail(email, pid):
    email = email.strip().lower()
    db.insert('emails', email=email, pid = pid)

def del_pat2mail(email, pid):
    email = email.strip().lower()
    db.delete('emails', where="email=$email and pid=$pid", vars=locals())

def get_maillist():
    return db.query("select email from emails group by email order by email")

def del_mail(email):
    db.delete('emails', where="email=$email", vars=locals())
