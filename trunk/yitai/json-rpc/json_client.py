# -*- coding: utf-8 -*-
# yitai_json.py --- Time-stamp: <Julian Qian 2010-10-28 13:33:10>
# Copyright 2010 Julian Qian
# Author: jqian@desktop
# Version: $Id: yitai_json.py,v 0.0 2010/10/11 02:33:57 jqian Exp $
# Keywords:

# curl -H 'Content-Type: application/json' -d "{'method':'login','params':['test_1','b0baee9d279d34fa1dfd71aadb908c3f'],'id':1}" http://rpc.ehomeonline.net

import urllib2

class Yitai:
    def __init__(self):
        self.request_id = 0

    def request(self, method, params):
        self.request_id += 1

        req = urllib2.Request(url = "http://rpc.ehomeonline.net",
                              headers = {"Content-type": "application/json",
                                         "Accept": "text/plain"},
                              data = '{"method":"' + method + '","params":[' + params + '],"id": ' + str(self.request_id) + '}'
                              )
        f = urllib2.urlopen(req)

        print f.read()

def main():
    "yitai_json.py"
    yitai = Yitai()
    yitai.request("test", '')
    yitai.request("login", '"test_1","b0baee9d279d34fa1dfd71aadb908c3f"')
    yitai.request("getDeviceList", '')
    yitai.request("getBindPhones", '1')
    # yitai.request("setBindPhone", '1,"18602419635",{"urgency":0}')
    # yitai.request("setBindPhone", '1,"18602419636",{"urgency":1}')
    yitai.request("delBindPhone", '1,"18602419636"')
    yitai.request("getBindPhones", '1')
    # yitai.request("getDeviceStatus", '1')
    # yitai.request("checkAlarmMessage", '')


if __name__ == "__main__":
    main()
