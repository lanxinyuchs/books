#!/usr/bin/env python
import json, sys, urllib2, argparse, re
from os import getenv
URL=getenv('ER_URL', "https://er001-eiffel002.rnd.ki.sw.ericsson.se:8443/eventrepository/restapi")+"/events"

parser = argparse.ArgumentParser(description='Query Event Repository')
parser.add_argument('-q', help='specify query')
parser.add_argument('-i', help='iteam number', default=0)
parser.add_argument('-v', help='user specific version [not supported]', default="use latest")
parser.add_argument('path', metavar='PATH', type=str, nargs="*",
      help='json path to extract, i.e. eventData, gav, version')
args = parser.parse_args()

# compare two eiffelMessageVersions.
def cmpVer(v1s, v2s):
   v1= v1s.split('.')
   v2= v2s.split('.')
   for i in range(0, min(len(v1), len(v2))):
      if v1[i] != v2[i]: return cmp(v1[i], v2[i])
   return cmp(len(v1), len(v2))

proxy_handler = urllib2.ProxyHandler({})
opener = urllib2.build_opener(proxy_handler)
req = urllib2.Request(URL+args.q)
response = opener.open(req)
data = json.load(response)
if "items" in data.keys():
   data = data["items"][int(args.i)]
data = data["eiffelMessageVersions"]
eiffelMessageVersions = data.keys()
eiffelMessageVersions.sort(cmp=cmpVer)
data = data[eiffelMessageVersions[-1]]
path = []
for i in args.path:
   if re.match("^[0-9]+$", i):
      path.append(i)
   elif re.match('^".*"$', i):
      path.append(i)
   else:
      path.append('"%s"'%i)
print eval("data"+"".join(map(lambda x:"[%s]"%x, path)))
