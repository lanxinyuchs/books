#!/usr/bin/env python
import json
import os
from subprocess import check_output

erUrl = os.getenv('ER_URL',
                  'https://er001-eiffel002.rnd.ki.sw.ericsson.se:8443/eventrepository/restapi')
erCommand = 'er_client.py -u {} --json'.format(erUrl)


class ArtifactBase:
    """
    Base class for Budit-managed Eiffel artifacts.
    """
    _group = 'com.ericsson'
    _id = ''
    _version = ''
    _ane = ''
    _depsPopulated = False
    _packageUrl = ''
    _productName = ''
    _provides = []
    _contains = []

    def _populateAne(self):
        if self._ane:
            return
        self._ane = get_ane_event(self.id(), self.version())
        try:
            self._packageUrl = self._ane["eventData"]['optionalParameters'][
                'packageurl']
        except:
            pass
        try:
            self._productName = self._ane['eventData']['optionalParameters'][
                'productname']
        except:
            pass

    def __init__(self, group, artifact, version):
        self._group = group
        self._id = artifact
        self._version = version

    def group(self):
        return self._group

    def id(self):
        return self._id

    def version(self):
        return self._version

    def provides(self):
        return self._provides

    def contains(self):
        return self._contains

    def fullName(self):
        if self.productName():
            return '{}_{}'.format(self.productName(), self.id())
        return self.id()

    def gav(self):
        return '{}:{}:{}'.format(self.group(), self.fullName(), self.version())

    def type(self):
        """Abstract method"""
        raise NotImplementedError

    def packageUrl(self):
        self._populateAne()
        return self._packageUrl

    def productName(self):
        self._populateAne()
        return self._productName


class Artifact(ArtifactBase):
    """
    Class representing Eiffel non-interface artifacts.
    """

    def _populateDependencies(self):
        """ Populates "provides" and "contains" lists """
        if self._depsPopulated:
            return
        self._depsPopulated = True
        self._populateAne()
        if self._ane is None:
            return
        event = process(self._ane['eventId'], 'BaselineDefinedEvent',
                        ['baselineName'],
                        'Baseline for {}'.format(self.id()))
        for entry in event['eventData']['consistsOf']:
            if 'tag' not in entry:
                continue
            tag = entry['tag']
            if tag == 'provides':
                self._provides.append(make_class(entry['gav']['groupId'],
                                                 entry['gav']['artifactId'],
                                                 entry['gav']['version']))
            elif tag == 'contains':
                self._contains.append(make_class(entry['gav']['groupId'],
                                                 entry['gav']['artifactId'],
                                                 entry['gav']['version']))

    def provides(self):
        self._populateDependencies()
        return self._provides

    def contains(self):
        self._populateDependencies()
        return self._contains

    def type(self):
        return 'artifact'


class Interface(ArtifactBase):
    """
    Class representing an interface product (CXA). Interface products lack
    "contains" and "provides" dependencies.
    """

    def type(self):
        return 'interface'


def make_class(group, artifact, version):
    if artifact.startswith('CXA'):
        return Interface(group, artifact, version)
    return Artifact(group, artifact, version)


# Helper functions

def get_ane_event(artifact, version):
    """Return the ANE for the given artifact and version"""
    try:
        d = check_output(
            erCommand + ' findArtifactNewEvent artifactId={} version={}'.format(
                artifact, version), shell=True)
        return json.loads(d)
    except:
        return None


def get_event(id):
    """Returns the Eiffel event with the given id"""
    d = check_output(erCommand + (' getEvent eventId={}'.format(id)),
                     shell=True)
    try:
        return json.loads(d)
    except:
        return None


def deep_search(dataStructure, path, value):
    """
    Go down the structure until path is found and match against value
    """
    try:
        child = dataStructure[path[0]]
        duppath = path[1:]
        if duppath:
            return deep_search(child, duppath, value)
        return child == value
    except:
        return False


def process(id, type, path, value):
    """
    Searches recursively down Eiffel messages until a message with has
    a <path> with <value> is found, or no more messages can be searched.
    The <path> is a list of keys/indexes inside the eventData structure, i.e
    "eventData" should not be part of the list.
    
    :param id Eiffel eventId to start search from
    :param type is the message type to find, e.g. "ArtifactNewEvent"
    :param path is the path to the data, e.g. ['eventData', 'flowContext']
    :param value is the value to match
    :returns the first matching event as a data structure, or None.
    """
    ids = [id]
    while ids:
        id = ids.pop(0)
        event = get_event(id)
        if event is None:
            continue
        if event["eventType"] == 'Eiffel{}'.format(type):
            if deep_search(event["eventData"], path, value):
                return event
        ids += event["inputEventIds"]
