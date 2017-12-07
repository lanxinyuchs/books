#!/usr/bin/env python
'''
This program finds an Eiffel event recursively through input event ids

find-event.py <event id> <event type> <data item path> <value>

Example:

    find-event.py 3e4728d1-ed42-4b61-bf46-53cfab7e20c0 ArtifactNewEvent gav artifactId CXA11448_2

'''

import json
import os
from subprocess import check_output
import sys

erUrl=os.getenv('ER_URL', 'https://er001-eiffel002.rnd.ki.sw.ericsson.se:8443/eventrepository/restapi')
erCommand='er_client.py -u {} --json getEvent eventId='.format(erUrl)
erAneCommand='er_client.py -u {} --json findArtifactNewEvent artifactId={{}} version={{}}'.format(erUrl)

def get_event(id):
    d=check_output('{}{}'.format(erCommand, id), shell=True)
    try:
        return json.loads(d)
    except:
        return None

def get_ane_event(artifact_id,version):
    d=check_output(erAneCommand.format(artifact_id, version), shell=True)
    try:
        return json.loads(d)
    except:
        return None


seen={}

def deepSearch(dataStructure, path, value):
    '''Go down the structure until path is found and match against value'''
    try:
        child=dataStructure[path[0]]
        duppath = path[1:]
        if duppath:
                return deepSearch(child, duppath, value)
        return child == value
    except:
        return False

def process(id, type, path, value):
    ids=[id]
    while ids:
        id=ids.pop(0)
        event=get_event(id)
        if event is None: continue
        if id in seen:
            continue
        seen[id] = 1
        if event["eventType"] == 'Eiffel{}'.format(type):
            if deepSearch(event["eventData"], path, value):
                print json.dumps(event)
                return
        try:
            for member in event['eventData']['consistsOf']: 
                if isinstance(member, dict):
                    event=get_ane_event(member['gav']['artifactId'],member['gav']['version'])
                    if event:
                        ids += [event['eventId']]
                else:
                    ids += [member]
        except: pass
        ids += event["inputEventIds"]

if len(sys.argv) < 5:
    print "Usage: find-event.py type path value\nExample: find-event.py aaaaaaaa-bbbb-cccc-dddd ArtifactNewEvent gav artifactId CXP1234567"
    sys.exit()

id = sys.argv[1]
type=sys.argv[2]
path=sys.argv[3:-1]
value=sys.argv[-1]

process(id, type, path, value)



