#!/usr/bin/env python3

# A Recursive Downloader of Gopher Menus

from getopt import getopt, GetoptError
from urllib.parse import urlsplit
from sys import argv
import time
import socket
import os

class Config():

    getopt_spec = "l:w:hrspcdm"

    def __init__(self, optdict):
        # Commandline options
        flags = optdict.keys()
        self.recursive = True if '-r' in flags else False
        self.maxdepth = 8 if not '-l' in flags else int(optdict['-l'])
        self.spanhosts = True if '-s' in flags else False
        self.helpme = True if '-h' in flags else False
        self.clobber = True if '-c' in flags else False
        self.only_menu = True if '-m' in flags else False
        self.ascend_parents = False if not '-p' in flags else True
        self.delay = 0.0 if not '-w' in flags else float(optdict['-w'])
        self.debug = True if '-d' in flags else False

    def __str__(self):
        lst = ["\n  recursive = %s" % self.recursive,
               "  maxdepth = %s" % self.maxdepth,
               "  spanhosts = %s" % self.spanhosts,
               "  helpme = %s" % self.helpme,
               "  clobber = %s" % self.clobber,
               "  only_menu = %s" % self.only_menu,
               "  ascend_parents = %s" % self.ascend_parents,
               "  delay = %s" % self.delay,
               "  debug = %s" % self.debug]

        return "\n".join(lst)


def print_options():
    helpdoc = {"-r" : "Enable recursive downloads",
               "-l [depth]" : "Maximum depth in recursive downloads (default 8)",
               "-s" : "Span hosts on recursive downloads",
               "-h" : "Show this help",
               "-c" : "Enable file clobbering",
               "-m" : "Only download gopher menus",
               "-p" : "Allow ascension to the parent directories",
               "-w [seconds]" : "Delay between downloads",
               "-d" : "Enable debug messages"}

    for (key, value) in helpdoc.items():
        print("  {} {}".format(key, value))


def debug(msg, config):
    if config.debug:
        print("debug: {}".format(msg))

class GopherURL():
    invalid_types = ['7',         # Search service
                     '2',         # CSO
                     '3',         # Error
                     '8', 'T']   # telnet

    def __init__(self, type, text, path, host, port):
        self.host = host
        self.port = port
        self.path = path
        self.text = text
        self.type = type

    def __str__(self):
        string = "<GopherURL ({})({})({})({})({})>".format(
                 self.type, self.text, self.host, self.path, self.port)
        return string

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
        sock.send(bytes(self.path + "\r\n", "US-ASCII"))
        buffer = bytearray()
        data = None
        while data != b'':
            data = sock.recv(2048)
            buffer.extend(data)
        sock.close()
        return buffer

def getlinks(pagecontent, config, roothost, rootpath):
    urls = []
    for line in pagecontent.split(sep='\n'):
        tokens = line.strip().split(sep='\t')
        try:
            type = tokens[0][0]
            text = tokens[0][1:]
            path = tokens[1].strip()
            host = tokens[2].strip()
            port = int(tokens[3].strip())

            if config.spanhosts and host != roothost:
                debug("Not spanning host: {} != {}".format(host, roothost), config)
                continue

            if not config.ascend_parents and not rootpath in path:
                debug("Not ascending parents: {} not in {}".format(rootpath, path), config)
                continue

            url = GopherURL(type, text, path, host, port)

            if not url.valid():
                continue

            urls.append(url)

        except IndexError:
            debug("Invalid line (IndexError)", config)
        except ValueError as e:
            debug("Invalid Port: {}".format(e), config)

    return urls

def print_help_quit(ret):
    print("Usage: gopherdl.py [options] [url1 url2 ...]")
    print("Options:")
    print_options()
    quit(ret)

def mkdirs(path):
    at = ""
    for p in path.split(os.path.sep):
        at = os.path.join(at, p)
        if not os.path.exists(at):
            os.mkdir(at)

def write_gopherurl(gurl, config, content = None):
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

    content = content if content != None else gurl.download(config.delay)

    with open(outfile, "wb") as outfile:
        outfile.write(content)

# Return a tuple, (host,port,path)
def spliturl(urlstr):

    has_gopher_scheme = urlstr[0:9] == "gopher://"
    has_scheme = "://" in urlstr

    # They specified an incompatible protocol
    if has_scheme and not has_gopher_scheme:
        raise ValueError

    # Assume they meant gopher, give it an empty scheme
    if not has_scheme:
        urlstr = "//{}".format(urlstr)

    url = urlsplit(urlstr)
    path = url.path
    host = url.netloc
    port = 70 if url.port is None else url.port

    return (host, port, path)

# Returns a list of valid gopher URLS pointing to content
# Menus are saved to files to prevent duplicate downloads
def crawl_recursively(gurl, depthleft, config, rootgurl, path_history):

    if gurl.path in path_history:
        return []

    if depthleft == 0:
        return []

    depthleft = None if depthleft is None else (depthleft-1)

    # A gopher menu
    if gurl.type == '1':
        content = gurl.download(config.delay)
        next_gurls = getlinks(content.decode("utf-8", errors="ignore"), config, rootgurl.host, rootgurl.path)
        # To Avoid duplicate downloads, pass this menu to save
        write_gopherurl(gurl, config, content = content)
        path_history.append(gurl.path)

        for next_gurl in next_gurls:
            debug(next_gurl, config)
            if next_gurl.path in path_history:
                continue
            path_history.append(next_gurl.path)
            next_gurls.extend(crawl_recursively(next_gurl, depthleft, config, rootgurl, path_history))

        return next_gurls
    else:
        return [gurl]

def main():

    optlist, args = ([], [])

    try:
        optlist, args = getopt(argv[1:], Config.getopt_spec)
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
            host, port, path = spliturl(host)
            root_gurl_type = "1" if config.recursive else "0"
            rootgurl = GopherURL(root_gurl_type, "[ROOT URL]", path, host, port)

            if config.recursive:
                gopher_urls = crawl_recursively(rootgurl, config.maxdepth, config, rootgurl, [])

                if not config.only_menu:
                    for gurl in gopher_urls:
                        write_gopherurl(gurl, config)

            # Single file download
            else:
                write_gopherurl(rootgurl, config)
        except ValueError as e:
            print(e)

main()
