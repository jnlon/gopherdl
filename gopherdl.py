#!/usr/bin/env python3

# gopherdl.py

import getopt
import sys
import math
import urllib.parse
import time
import socket
import os
import re

# TOFIX: content=None on write_gopherurl is a bad codesmell, this function
# should only be called in one place

class Config():

    getopt_spec = "l:w:hrspcdmnA:R:M"

    def __init__(self, optdict):
        # Commandline options
        flags = optdict.keys()
        self.recursive = '-r' in flags
        self.maxdepth = math.inf if not '-l' in flags else int(optdict['-l'])
        self.spanhosts = '-s' in flags
        self.helpme = '-h' in flags
        self.clobber = '-c' in flags
        self.only_save_menu = '-m' in flags
        self.no_save_menu = '-n' in flags
        self.ascend_parents = '-p' in flags
        self.delay = 0.0 if not '-w' in flags else float(optdict['-w'])
        self.debug = '-d' in flags
        self.accept_regex = None if not '-A' in flags else re.compile(optdict['-A'])
        self.reject_regex = None if not '-R' in flags else re.compile(optdict['-R'])
        self.regex_on_menus = '-M' in flags

    def __str__(self):
        lst = [ "  recursive = %s" % self.recursive,
                "  maxdepth = %s" % self.maxdepth,
                "  spanhosts = %s" % self.spanhosts,
                "  helpme = %s" % self.helpme,
                "  clobber = %s" % self.clobber,
                "  only_save_menu = %s" % self.only_save_menu,
                "  ascend_parents = %s" % self.ascend_parents,
                "  delay = %s" % self.delay,
                "  debug = %s" % self.debug,
                "  accept_regex = %s" % self.accept_regex,
                "  reject_regex = %s" % self.reject_regex,
                "  regex_on_menus = %s" % self.regex_on_menus ]

        return "\n".join(lst)

def print_options():
    helpdoc = { "-r" : "Enable recursive downloads",
                "-l [depth]" : "Maximum depth in recursive downloads (default none)",
                "-s" : "Span hosts on recursive downloads",
                "-h" : "Show this help",
                "-c" : "Enable file clobbering (overwrite existing)",
                "-m" : "Only download gopher menus",
                "-n" : "Never download gopher menus",
                "-p" : "Allow ascension to the parent directories",
                "-w [seconds]" : "Delay between downloads",
                "-d" : "Enable debug messages",
                "-A" : "Accept URL regex",
                "-R" : "Reject URL regex",
                "-M" : "Apply accept/reject regex rules to menus (can prevent recursion)" }

    for (key, value) in helpdoc.items():
        print("  {} {}".format(key, value))

def debug(msg, config):
    if config.debug:
        print("debug: {}".format(msg))

class GopherURL():
    invalid_types = ['7',         # Search service
                     '2',         # CSO
                     '3',         # Error
                     '8', 'T']    # telnet

    def __init__(self, type, text, path, host, port):
        self.host = host
        self.port = port
        self.path = path
        self.text = text
        self.type = type

    def __str__(self):
        s = '<GopherURL [{}]({})({})({})>'
        return s.format(self.type, self.host, self.port, self.path)

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            samehost = self.host == other.host
            samepath = self.path == other.path
            return samehost and samepath
        return False

    def __hash__(self):
        return hash(self.to_file_path())

    def valid(self):
        if len(self.path) == 0:
            return False
        if self.port <= 0:
            return False
        if self.type in GopherURL.invalid_types:
            return False
        if 'URL:' in self.path:
            return False

        # If the path contains enough "../", it would be saved outside our
        # download directory, which is a security risk. Ignore these files
        file_path = os.path.relpath(self.to_file_path())
        in_download_dir = file_path.startswith(self.host)

        if not in_download_dir: 
            return False

        return True

    def download(self, delay):
        while True:
            time.sleep(delay)
            sock = socket.socket(family=socket.AF_INET, type=socket.SOCK_STREAM)
            buffer = bytearray()
            try:
                sock.connect((self.host, self.port))
                sock.send(bytes(self.path + "\r\n", "utf-8"))
                data = None
                while data != b'':
                    data = sock.recv(1024)
                    buffer.extend(data)
                sock.close()
            except ConnectionRefusedError:
                print("Connection refused from {}:{}{}, retrying...".format(self.host, self.port, self.path))
                delay = delay if delay != 0 else 1
                continue
            return buffer

    def is_menu(self):
        return self.type == '1'

    # As it would look in a browser urlbar
    def to_url(self):
        return urllib.parse.urlunparse(('gopher', self.host, self.path, '', '', ''))

    # path without adding gophermap
    def to_url_path(self):
        path = self.path.strip("/").split("/")
        outfile = os.path.join(self.host, os.path.sep.join(path))
        return outfile

    def to_file_path(self):
        outfile = self.to_url_path()
        if self.is_menu():
            outfile = os.path.join(outfile, "gophermap")
        return outfile


def debug_list(lst, message, config):
    debug(message, config)
    for item in lst:
        debug(item, config)

def print_help_quit(ret):
    print("Usage: gopherdl.py [options] [url1 url2 ...]")
    print("Options:")
    print_options()
    quit(ret)

def mkdirs(path):
    cd = str()
    for p in path.split(os.path.sep):
        cd = os.path.join(cd, p)
        if not os.path.exists(cd):
            os.mkdir(cd)

def get_menus(gurls): 
    return [ g for g in gurls if g.is_menu() ]

def get_files(gurls): 
    return [ g for g in gurls if not g.is_menu() ]

def slurp(path):
    with open(path, "rb") as f:
        return f.read()

# Extract urls from a gopher menu
def getlinks(menucontent, config):
    urls = []
    for line in menucontent.split(sep='\n'):
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

        except IndexError:
            debug("Invalid line (IndexError)", config)
        except ValueError as e:
            debug("Invalid Port: {}".format(e), config)

    debug_list(urls, "All urls: {}".format(len(urls)), config)

    return urls

def write_gopherurl(gurl, config, content=None):

    outfile = gurl.to_file_path()

    # If it exists and config says no clobber, leave
    if os.path.exists(outfile) and not config.clobber:
        print("Not overwriting:", outfile)
        return

    mkdirs(os.path.dirname(outfile))
    content = content if content != None else gurl.download(config.delay)

    debug("write_gopherurl: {}".format(gurl), config)
    with open(outfile, "wb") as outfile:
        outfile.write(content)

# Return a tuple, (host,port,path)
def spliturl(urlstr):

    has_gopher_scheme = urlstr[0:9] == "gopher://"
    has_scheme = "://" in urlstr

    # They specified an incompatible protocol
    if has_scheme and not has_gopher_scheme:
        raise ValueError("Invalid scheme in url '{}'".format(urlstr))

    # Assume they meant gopher, give it an empty scheme
    if not has_scheme:
        urlstr = "//{}".format(urlstr)

    url = urllib.parse.urlsplit(urlstr)
    path = "/" if len(url.path) == 0 else url.path
    host = url.netloc.split(":")[0]
    port = 70 if url.port is None else url.port

    return (host, port, path)

def crawl(root_gurl, config):

    def gurl_ok_by_config(link):

        on_different_host = root_gurl.host != link.host
        if not config.spanhosts and on_different_host:
            debug("Not spanning: {} != {}".format(root_gurl.host, link.host), config)
            return False

        off_original_path = not link.path.startswith(root_gurl.path)
        if not config.ascend_parents and off_original_path:
            debug("Not Ascending: {} <-> {}".format(root_gurl.path, link.path), config)
            return False

        # If config says not to apply regex on menus, stop here if it is
        # If the link is a menu AND we don't apply regex on menus, return
        if not config.regex_on_menus and link.is_menu():
            return True

        # Filter by regular expressions
        url = link.to_url()

        if config.reject_regex != None: 
            match = config.reject_regex.fullmatch(url)
            if match != None:
                debug("Reject: {}".format(url), config)
                return False

        if config.accept_regex != None:
            match = config.accept_regex.fullmatch(url)
            if match != None:
                debug("Accept: {}".format(url), config)
                return True
            else:
                return False

        return True

    def retrieve_menu_content(gurl):
        path = gurl.to_file_path()
        content = None
        if os.path.exists(path) and not config.clobber:
            print(":: Using existing menu {}".format(path))
            content = slurp(path)
        else:
            content = gurl.download(config.delay)
        return content.decode('utf-8', errors='ignore') 

    def gopher_urls_from_menu_link(menu_gurl):
        debug(menu_gurl, config)
        menu_content = retrieve_menu_content(menu_gurl)

        gurls = getlinks(menu_content, config)
        debug_list(gurls, "Before filter # urls: {}".format(len(gurls)), config)

        gurls = list(filter(gurl_ok_by_config, gurls))
        debug_list(gurls, "After filter # urls: {}".format(len(gurls)), config)

        return gurls

    gurls = set(gopher_urls_from_menu_link(root_gurl))
    menus = list(set(get_menus(gurls))) + [root_gurl]
    depth = 0

    for menu in menus:
        if depth > config.maxdepth:
            debug("Maxdepth {} reached".format(config.maxdepth), config)
            break

        new_gurls = set(gopher_urls_from_menu_link(menu))
        new_unique_gurls = new_gurls.difference(gurls)
        gurls.update(new_unique_gurls)
        new_unique_gurls_menus = get_menus(new_unique_gurls)
        menus.extend(new_unique_gurls_menus)
        depth += 1

        print("{} | {}/{} | {} ".format( len(gurls), depth, len(menus), menu.to_url_path()))

    return gurls

def download_gopher_urls(gopher_urls, config):
    for i in range(len(gopher_urls)):
        gurl = gopher_urls[i]
        print("[{}/{}] {}".format((i + 1), len(gopher_urls), gurl.to_file_path()))
        write_gopherurl(gurl, config)

def gopherdl(host, config):

    # hueristic: probably a menu if there's no file extension or ends in /
    def probably_a_menu(path):
        end = path.split("/")[-1]
        return not "." in end or path[-1] == '/'

    host, port, path = spliturl(host)
    root_gurl_type = "1" if probably_a_menu(path) else "0"
    root_gurl = GopherURL(root_gurl_type, "[ROOT URL]", path, host, port)
    debug("root_gurl: {}".format(root_gurl), config)

    if config.recursive:
        # Recursive download
        print(":: Downloading menu tree")
        gopher_urls = crawl(root_gurl, config)
        gopher_files = get_files(gopher_urls)
        gopher_menus = get_menus(gopher_urls)

        if config.no_save_menu:
            gopher_menus = []

        if config.only_save_menu:
            gopher_files = []

        if gopher_urls == []:
            print(":: Nothing to download")
            return

        if len(gopher_menus) > 0:
            print(":: Downloading {} menus".format(len(gopher_menus)))
            download_gopher_urls(gopher_menus, config)

        if len(gopher_files) > 0:
            print(":: Downloading {} files".format(len(gopher_files)))
            download_gopher_urls(gopher_files, config)

    else: 
        # Single file download
        print(":: Downloading single file ")
        print(root_gurl.to_file_path())
        write_gopherurl(root_gurl, config)


def main():

    optlist, args = ([], [])
    try:
        optlist, args = getopt.getopt(sys.argv[1:], Config.getopt_spec)
    except getopt.GetoptError:
        print_help_quit(1)

    optdict = dict(optlist)
    config = Config(optdict)
    hosts = args

    debug("\n{}".format(config), config)

    if config.helpme:
        print_help_quit(0)
    elif hosts == []:
        print_help_quit(1)

    for host in hosts:
        try:
            gopherdl(host, config)
        except ValueError as e:
            print(e)

if __name__ == "__main__":
    main()
