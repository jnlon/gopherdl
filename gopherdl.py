#!/usr/bin/env python3

# A Recursive Downloader of Gopher Menus

import socket
from getopt import getopt
from sys import argv
from enum import Enum
from urllib.parse import urlsplit

class GopherURL():
    invalid_types = [ '7',         # Search service
                      '2',         # CSO
                      '3',         # Error
                      '8', 'T' ]   # telnet

    def __init__(self, type, text, path, host, port):
        self.host = host
        self.port = port
        self.path = path
        self.text = text
        self.type = type

    def __str__(self):
        s = "<GopherURL ({})({})({})({})({})>".format(self.type, self.text, self.host, self.path, self.port)
        return s

    def valid(self):
        if len(self.path) == 0:
            return False
        if self.type in GopherURL.invalid_types:
            return False
        return True

def download(host, port, path = ""):
    sock = socket.socket(family=socket.AF_INET, type=socket.SOCK_STREAM)
    sock.connect((host, port))
    sock.send(bytes(path + "\n", "US-ASCII"))
    buffer = bytearray()
    data = False
    while data != b'':
        data = sock.recv(2048)
        buffer.extend(data)
    sock.close()
    return buffer

# Args: -l <depth>
# 

def getlinks(pagecontent, currenthost, spanhosts=False):
    urls = []
    for line in pagecontent.split(sep='\n'):
        tokens = line.strip().split(sep='\t')
        try:
            type = tokens[0][0]
            text = tokens[0][1:]
            path = tokens[1].strip()
            host = tokens[2].strip()
            port = int(tokens[3].strip())

            url = GopherURL(type, text, path, host, port)

            if not url.valid():
                continue

            urls.append(url)
        except IndexError as e:
            print("Invalid line, skipping")
        except ValueError as e:
            print("Invalid port: ", e)

    return urls

optlist,args = getopt(argv[1:], "l:sr")
optdict = dict(optlist)

recursive = True if "-r" in optdict.keys() else False
maxdepth = 1 if not "-l" in optdict.keys() else int(optdict['-l'])
spanhosts = True if "-s" in optdict.keys() else False
hosts = args

# Return a tuple, (host,port,path)
def spliturl(url):
    if url[0:9] != "gopher://":
        url = "gopher://" + url
    up = urlsplit(url)
    path = up.path
    host = up.netloc
    try:
        s = host.split(sep=":")
        port = int(s[1])
        host = s[0]
    except (ValueError, IndexError):
        port = 70
        host = up.netloc
    return (host, port, path)

print(args, optdict)

for host in hosts:
    host,port,path = spliturl(host)
    #print(host, "--", port, "--", path)
    content = download(host,port,path=path).decode("US-ASCII")
    urls = getlinks(content, host, spanhosts=spanhosts)
    for url in urls:
        print(url)

#print(download("gopher.floodgap.com", 70, path="calroads").decode("US-ASCII"))

# Program assumes first page is a menu

#for url in urls:
#    print(url)


