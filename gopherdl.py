#!/usr/bin/env python3

# A Recursive Downloader of Gopher Menus

import socket
from getopt import getopt,GetoptError
from sys import argv
from enum import Enum
from urllib.parse import urlsplit
import os

class Config():
    def __init__(self, optdict):
        self.recursive = True if "-r" in optdict.keys() else False
        self.maxdepth = 1 if not "-l" in optdict.keys() else int(optdict['-l'])
        self.spanhosts = True if "-s" in optdict.keys() else False
        self.helpme = True if "-h" in optdict.keys() else False
        self.clobber = True if "-c" in optdict.keys() else False

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

    def download(self):
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
            pass
            #print("Invalid line, skipping")
        except ValueError as e:
            pass
            #print("Invalid port: ", e)

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

def mkdirs(path):
    at = ""
    for p in path.split(os.path.sep):
        at = os.path.join(at, p)
        if not os.path.exists(at):
            os.mkdir(at)


def savefile(gurl, clobber):
    path = gurl.path.strip("/").split("/")
    outfile = os.path.join(gurl.host, os.path.sep.join(path))

    if gurl.type == '1': 
        outfile = os.path.join(outfile, "gophermap")

    mkdirs(os.path.dirname(outfile))

    print(outfile)

    # If it exists and we can't clobber, leave
    if os.path.exists(outfile) and not clobber:
        return

    with open(outfile, "wb") as f:
        f.write(gurl.download())

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

def download_recursively(gurl, depthleft, config):

    if depthleft == 0:
        return

    depthleft = None if depthleft is None else (depthleft-1)

    content = gurl.download()
    savefile(gurl, config.clobber)

    if gurl.type == '1': # A gopher menu
        gurls = getlinks(content.decode("us-ascii"), gurl.host, config.spanhosts)
        for g in gurls:
            download_recursively(g, depthleft, config)

def main():

    optlist,args = [],[] 

    try:
        optlist,args = getopt(argv[1:], "l:hrspc")
    except GetoptError: 
        printhelp_quit(1)

    optdict = dict(optlist)
    config = Config(optdict)
    hosts = args

    if config.helpme:
        printhelp_quit(0)
    elif hosts == []:
        printhelp_quit(1)

    for host in hosts:
        host,port,path = spliturl(host)
        rootgurl = GopherURL("1", "", path, host, port)

        if config.recursive:
            download_recursively(rootgurl, config.maxdepth, config)
        else:
            content = rootgurl.download()
            savefile(rootgurl, config.clobber)

        #for gurl in gurls:
        #    #print(gurl)
        #    savefile(gurl.download(), gurl.type, gurl.host, gurl.path)

main()
# print(download("gopher.floodgap.com", 70, path="calroads").decode("US-ASCII")).
# Program assumes first page is a menu
#for url in urls:
#    print(url)


