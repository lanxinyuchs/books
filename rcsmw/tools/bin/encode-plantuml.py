#!/usr/bin/env python

"""
"""
import zlib
import sys

__version__ = [0,1,1]
__version_string__ = '.'.join(str(x) for x in __version__)

__author__ = 'Doug Napoleone'
__email__ = 'doug.napoleone+plantuml@gmail.com'

# Modified by Gunnar.Strand@ericsson.com 2016.
# CAUTION: Do NOT distribute outside Ericsson AB unless approved by Ericsson
# OSS board!

#: Default plantuml service url
SERVER_URL = 'http://plantuml.edd.ericsson.se:8000/plantuml/img/'

class PlantUMLError(Exception):
    """Error in processing.
    """
    pass

class PlantUMLConnectionError(PlantUMLError):
    """Error connecting or talking to PlantUML Server.
    """
    pass

class PlantUMLHTTPError(PlantUMLConnectionError):
    """Request to PlantUML server returned HTTP Error.
    """
    def __init__(self, response, content, *args, **kwdargs):
        super(PlantUMLConnectionError, self).__init__(*args, **kwdargs)
        self.response = response
        self.content = content
        if not self.message:
            self.message = "%d: %s" % (
                self.response.status, self.response.reason)

def deflate_and_encode(plantuml_text):
    """zlib compress the plantuml text and encode it for the plantuml server.
    """
    zlibbed_str = zlib.compress(plantuml_text)
    compressed_string = zlibbed_str[2:-4]
    return encode(compressed_string)

def encode(data):
    """encode the plantuml data which may be compresses in the proper
    encoding for the plantuml server
    """
    res = ""
    for i in xrange(0,len(data), 3):
        if (i+2==len(data)):
            res += _encode3bytes(ord(data[i]), ord(data[i+1]), 0)
        elif (i+1==len(data)):
            res += _encode3bytes(ord(data[i]), 0, 0)
        else:
            res += _encode3bytes(ord(data[i]), ord(data[i+1]), ord(data[i+2]))
    return res

def _encode3bytes(b1, b2, b3):
    c1 = b1 >> 2;
    c2 = ((b1 & 0x3) << 4) | (b2 >> 4);
    c3 = ((b2 & 0xF) << 2) | (b3 >> 6);
    c4 = b3 & 0x3F;
    res = "";
    res += _encode6bit(c1 & 0x3F);
    res += _encode6bit(c2 & 0x3F);
    res += _encode6bit(c3 & 0x3F);
    res += _encode6bit(c4 & 0x3F);
    return res;

def _encode6bit(b):
    if b < 10:
        return chr(48 + b)
    b -= 10
    if b < 26:
        return chr(65 + b)
    b -= 26
    if b < 26:
        return chr(97 + b);
    b -= 26
    if b == 0:
        return '-'
    if b == 1:
        return '_'
    return '?'


class PlantUML(object):
    """Connection to a PlantUML server with optional authentication.

    All parameters are optional.

    :param str url: URL to the PlantUML server image CGI. defaults to
                    http://www.plantuml.com/plantuml/img/
    """
    def __init__(self, url=SERVER_URL):
        self.url = url if url else SERVER_URL

    def get_url(self, plantuml_text):
        """Return the server URL for the image.
        You can use this URL in an IMG HTML tag.

        :param str plantuml_text: The plantuml markup to render
        :returns: the plantuml server image URL
        """
        return self.url + deflate_and_encode(plantuml_text)


if __name__ == '__main__':
    url = (sys.argv[1] if len(sys.argv) > 2 else False)
    file = (sys.argv[2] if url else sys.argv[1])
    pl = PlantUML(url=url)
    with open(file) as input:
        print pl.get_url(input.read())
