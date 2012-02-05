# -*- coding: utf-8 -*-
# hjbbs.py --- Time-stamp: <Qian Julian 2012-02-05 18:49:11>
# Copyright 2010 Julian Qian
# Author: jqian@desktop
# Version: $Id: hjbbs.py,v 0.0 2010/06/23 09:45:25 jqian Exp $
# Keywords:

# How to run:
# $ python hjbbs.py 8000
# will run hjbbs.py with port 8000 as a small web server

from urllib import unquote
from daemon import createDaemon
import web
import model

### Url mappings

urls = (
    '/', 'Index',
    '/chk/?', 'Check',
    '/sub/?', 'Subscribe',
    '/del/?', 'Delete',
    '/pat/?(add|del)?', 'Pattern',
    '/mail/?(del)?', 'Email',
)


### Templates
render = web.template.render('templates', base='base')
snippet = web.template.render('templates')

class Index:
    def GET(self):
        """ Show page """
        # todos = model.get_todos()
        status = ""
        return render.index(status)

class Check:
    def POST(self):
        email = web.input(email=None).email
        pidlist = model.get_pid_by_email(email)
        pids = {}
        for itr in pidlist:
            pids[itr.pid] = True
        sublist = model.get_pattern()
        subs = []
        for pat in sublist:
            if pids.get(pat.pid):
                subs.append({"name": pat.name,
                             "pattern": pat.pattern,
                             "pid": pat.pid,
                             "cid": pat.cid,
                             "checked": " checked "})
            else:
                subs.append({"name": pat.name,
                             "pattern": pat.pattern,
                             "pid": pat.pid,
                             "cid": pat.cid,
                             "checked": ""})

        category = model.get_category()

        return snippet.sublist(category, subs)

class Subscribe:
    def POST(self):
        """
        """
        # pidlist = web.input(pid=[]).pid
        pid = web.input(patid=None).patid
        email = web.input(email=None).email
        model.new_pat2mail(email, pid)
        return "ok"

class Delete:
    def POST(self):
        """
        """
        # pidlist = web.input(pid=[]).pid
        pid = web.input(patid=None).patid
        email = web.input(email=None).email
        model.del_pat2mail(email, pid)
        return "ok"

class Pattern:
    """
    manager all pattern strings
    """
    def GET(self, action):
        patlist = self.patlist()
        category = model.get_category()
        return render.pattern(category, patlist)

    def POST(self, action):
        if action == "add":
            patname = web.input(patname=None).patname
            pattern = web.input(pattern=None).pattern
            category = web.input(category=None).category

            # if isinstance(patname, str):
            #     patname = patname.decode("utf-8")
            # if isinstance(pattern, str):
            #     pattern = pattern.decode("utf-8")

            model.new_pattern(patname, pattern, category)
        elif action == "del":
            pid = web.input(pid=None).pid
            model.del_pattern(pid)
        return self.patlist()

    def patlist(self):
        patterns = [{"name": pat.name,
                     "pattern": pat.pattern,
                     "pid": pat.pid,
                     "cid": pat.cid}
                    for pat in model.get_pattern()]
        return snippet.patlist(model.get_category(), patterns)

class Email:
    def GET(self, action):
        emails = model.get_maillist()
        return render.maillist(emails)

    def POST(self, action):
        if action == "del":
            email = web.input(email=None).email
            model.del_mail(email)

        return render.maillist(model.get_maillist())

app = web.application(urls, globals())

if __name__ == '__main__':
    createDaemon()
    app.run()
