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

    def download():
        sock = socket.socket(family=socket.AF_INET, type=socket.SOCK_STREAM)
        sock.connect((self.host, self.port))
        sock.send(bytes(self.path + "\n", "US-ASCII"))
        buffer = bytearray()
        data = None
        while data != b'':
            data = sock.recv(2048)
            buffer.extend(data)
        sock.close()
        return buffer

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

def printhelp_quit(ret):
    print("Usage: gopherdl.py [options] [url1 url2 ...]")
    print("Options:")
    helpdoc = { "-h" : "Show this help",
                "-r" : "Enable recursive downloads" ,
                "-l [depth]" : "Maximum depth in recursive downloads (default infinite)",
                "-s" : "Span hosts on recursive downloads",
                "-p" : "Allow ascension to the parent directories",
                "-c" : "Enable file clobbering" } 
    for (key,value) in helpdoc.items():
        print(" ",key,value)

    quit(ret)

# TODO: Implement saving to file! Top directory should be hostname
def savefile(content, host, path):
    pass

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

def download_recursively(gurl, maxdepth, spanhosts):

    if maxdepth == 0:
        return

    maxdepth = None if maxdepth is None else (maxdepth-1)

    content = gurl.download()
    savefile(content, gurl.host, gurl.path) # TODO: save to gophermap?

    if gurl.type == '1': # A gopher menu
        gurls = getlinks(content, gurl.host, spanhosts)
        for g in gurls:
            download_recursively(g, maxdepth, spanhosts)


optlist,args = getopt(argv[1:], "l:hrspc")
optdict = dict(optlist)
recursive = True if "-r" in optdict.keys() else False
maxdepth = 1 if not "-l" in optdict.keys() else int(optdict['-l'])
spanhosts = True if "-s" in optdict.keys() else False
helpme = True if "-h" in optdict.keys() else False
clobber = True if "-c" in optdict.keys() else False
hosts = args

if hosts == []:
    printhelp_quit(1)
elif helpme:
    printhelp_quit(0)

for host in hosts:
    host,port,path = spliturl(host)
    content = download(host,port,path=path).decode("US-ASCII")
    urls = getlinks(content, host, spanhosts=spanhosts)
    for url in urls:
        print(url)

#print(download("gopher.floodgap.com", 70, path="calroads").decode("US-ASCII"))

# Program assumes first page is a menu

#for url in urls:
#    print(url)


