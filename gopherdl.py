#!/usr/bin/env python3

# A Recursive Downloader of Gopher Menus

from getopt import getopt,GetoptError
from urllib.parse import urlsplit
from sys import argv
from time import sleep
import socket
import os

class Config():

    getopt_spec = "l:hrspcdm:w"

    def __init__(self, optdict):
        flags = optdict.keys()
        self.recursive = True if '-r' in flags else False
        self.maxdepth = 8 if not '-l' in flags else int(optdict['-l'])
        self.spanhosts = True if '-s' in flags else False
        self.helpme = True if '-h' in flags else False
        self.clobber = True if '-c' in flags else False
        self.only_menu = False if '-m' in flags else True
        self.ascend_parents = False if not '-p' in flags else True
        self.delay = 0.0 if not '-w' in flags else int(optdict['-w'])
        self.debug = True if '-d' in flags else False

    def __str__(self):
        l = [ "\n  recursive = %s" % self.recursive,
              "  maxdepth = %s" % self.maxdepth,
              "  spanhosts = %s" % self.spanhosts,
              "  helpme = %s" % self.helpme,
              "  clobber = %s" % self.clobber,
              "  only_menu = %s" % self.only_menu,
              "  ascend_parents = %s" % self.ascend_parents,
              "  delay = %s" % self.delay,
              "  debug = %s" % self.debug ]

        return "\n".join(l)

    def print_options():
        helpdoc = { "-r" : "Enable recursive downloads" ,
                    "-l [depth]" : "Maximum depth in recursive downloads (default 8)",
                    "-s" : "Span hosts on recursive downloads",
                    "-h" : "Show this help",
                    "-c" : "Enable file clobbering",
                    "-m" : "Only download gopher menus",
                    "-p" : "Allow ascension to the parent directories",
                    "-w [seconds]" : "Delay between downloads",
                    "-d" : "Enable debug messages"} 

        for (key,value) in helpdoc.items():
            print("  {} {}".format(key,value))

def debug(msg, config):
    if config.debug:
        print("debug: {}".format(msg))

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

    def download(self, delay):
        time.sleep(delay)
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

def getlinks(pagecontent, config, currenthost, base_path):
    urls = []
    for line in pagecontent.split(sep='\n'):
        tokens = line.strip().split(sep='\t')
        try:
            type = tokens[0][0]
            text = tokens[0][1:]
            path = tokens[1].strip()
            host = tokens[2].strip()
            port = int(tokens[3].strip())

            if not config.spanhosts and host != currenthost:
                debug("Not spanning host: {} != {}".format(host, currenthost), config.debug)
                continue

            if not config.ascend_parents and not base_path in path:
                debug("Not ascending parents: {} not in {}".format(base_path, path), config.debug)
                continue

            url = GopherURL(type, text, path, host, port)

            if not url.valid():
                continue

            urls.append(url)

        except IndexError:
            debug("Invalid line (IndexError)", config.debug)
        except ValueError as e:
            debug("Invalid Port: {}".format(e), config.debug)

    return urls

def print_help_quit(ret):
    print("Usage: gopherdl.py [options] [url1 url2 ...]")
    print("Options:")
    Config.print_options()
    quit(ret)

def mkdirs(path):
    at = ""
    for p in path.split(os.path.sep):
        at = os.path.join(at, p)
        if not os.path.exists(at):
            os.mkdir(at)


def write_gopherurl(gurl, config):
    debug("write_gopherurl: {}".format(gurl), config)
    path = gurl.path.strip("/").split("/")
    outfile = os.path.join(gurl.host, os.path.sep.join(path))

    if gurl.type == '1': 
        outfile = os.path.join(outfile, "gophermap")

    mkdirs(os.path.dirname(outfile))

    # If it exists and we can't clobber, leave
    if os.path.exists(outfile) and not config.clobber:
        print("Not overwriting:", outfile)
        return

    print("Writing:", outfile)
    with open(outfile, "wb") as f:
        f.write(gurl.download())

# Return a tuple, (host,port,path)
def spliturl(url):

    has_gopher_scheme = url[0:9] == "gopher://"
    has_scheme = "://" in url

    # They specified an incompatible protocol
    if has_scheme and not has_gopher_scheme:
        raise ValueError

    # Assume they meant gopher, give it an empty scheme
    if not has_scheme:
        url = "//{}".format(url)

    up = urlsplit(url)
    path = up.path
    host = up.netloc
    port = 70 if up.port == None else up.port

    return (host, port, path)

# Returns a list of valid gopher URLS pointing to content
# Menus are saved to files to prevent duplicate downloads
def crawl_recursively(gurl, depthleft, config):

    if depthleft == 0:
        return []

    depthleft = None if depthleft is None else (depthleft-1)

    if gurl.type == '1': # A gopher menu
        content = gurl.download(config.delay)
        gurls = getlinks(content.decode("utf-8"), gurl.host, config.spanhosts)

        # To Avoid duplicate downloads, download and save this menu
        write_gopherurl(gurl, config)
        
        for g in gurls:
            debug(g, config)
            gurls.extend(crawl_recursively(g, depthleft, config))

        return gurls

    else: # This is file content
        if gurl.only_menu: 
            return []
        else: 
            return [gurl]

def main():

    optlist,args = [],[] 

    try:
        optlist,args = getopt(argv[1:], Config.getopt_spec)
    except GetoptError: 
        print_help_quit(1)

    optdict = dict(optlist)
    config = Config(optdict)
    hosts = args

    debug("config: {}".format(config), config)

    if config.helpme:
        print_help_quit(0)
    elif hosts == []:
        print_help_quit(1)

    for host in hosts:
        try:
            host,port,path = spliturl(host)
            root_gurl_type = "1" if config.recursive else "0"
            rootgurl = GopherURL(root_gurl_type, "[ROOT URL]", path, host, port)

            if config.recursive:
                gopher_urls = crawl_recursively(rootgurl, config.maxdepth, config)
                for gu in gopher_urls:
                    write_gopherurl(gu, config)
            else: # Single file download
                content = rootgurl.download(config.delay)
                write_gopherurl(rootgurl, config)
        except ValueError:
            print("Invalid Host:", host)

main()
