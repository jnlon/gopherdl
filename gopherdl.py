#!/usr/bin/env python3

# gopherdl.py

from getopt import getopt, GetoptError
from urllib.parse import urlsplit
from sys import argv
from math import inf as INFINITY
import time
import socket
import os

class Config():

    getopt_spec = "l:w:hrspcdm"

    def __init__(self, optdict):
        # Commandline options
        flags = optdict.keys()
        self.recursive = '-r' in flags
        self.maxdepth = INFINITY if not '-l' in flags else int(optdict['-l'])
        self.spanhosts = '-s' in flags
        self.helpme = '-h' in flags
        self.clobber = '-c' in flags
        self.only_menu = '-m' in flags
        self.ascend_parents = '-p' in flags
        self.delay = 0.0 if not '-w' in flags else float(optdict['-w'])
        self.debug = '-d' in flags

    def __str__(self):
        lst = ["  recursive = %s" % self.recursive,
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
               "-l [depth]" : "Maximum depth in recursive downloads (default none)",
               "-s" : "Span hosts on recursive downloads",
               "-h" : "Show this help",
               "-c" : "Enable file clobbering (overwrite existing)",
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
        s = '<GopherURL [{}]({})({})({})>'
        return s.format(self.type, self.host, self.port, self.path)

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

    # path without adding gophermap
    def to_url_path(self):
        path = self.path.strip("/").split("/")
        outfile = os.path.join(self.host, os.path.sep.join(path))
        return outfile

    def to_file_path(self):
        outfile = self.to_url_path()
        if self.type == '1':
            outfile = os.path.join(outfile, "gophermap")
        return outfile

    def __eq__(self, other):
        if isinstance(other, self.__class__):
            samehost = self.host == other.host
            samepath = self.path == other.path
            return samehost and samepath
        return False


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

def slurp(path):
    with open(path, "rb") as f:
        return f.read()

def filter_duplicates(to_filter, items):
    return list(filter(lambda t: not t in items, to_filter))

def getlinks(pagecontent, config):
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

        except IndexError:
            debug("Invalid line (IndexError)", config)
        except ValueError as e:
            debug("Invalid Port: {}".format(e), config)

    debug_list(urls, "All urls: {}".format(len(urls)), config)

    return urls

def write_gopherurl(gurl, config, content=None):
    debug("write_gopherurl: {}".format(gurl), config)
    outfile = gurl.to_file_path()
    mkdirs(os.path.dirname(outfile))

    # If it exists and we can't clobber, leave
    if os.path.exists(outfile) and not config.clobber:
        print("Not overwriting:", outfile)
        return

    content = content if content != None else gurl.download(config.delay)

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

    url = urlsplit(urlstr)
    path = "/" if len(url.path) == 0 else url.path
    host = url.netloc.split(":")[0]
    port = 70 if url.port is None else url.port

    return (host, port, path)

def crawl(rootgurl, config):

    def gurl_ok_config(link):
        on_different_host = rootgurl.host != link.host
        if not config.spanhosts and on_different_host:
            debug("Not spanning: {} != {}".format(rootgurl.host, link.host), config)
            return False

        off_original_path = not link.path.startswith(rootgurl.path)
        if not config.ascend_parents and off_original_path:
            debug("Not Ascending: {} <-> {}".format(rootgurl.path, link.path), config)
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
            write_gopherurl(gurl, config, content=content)
        return content.decode('utf-8', errors='ignore') 

    def is_menu(gurl): return gurl.type == '1'
    def not_menu(gurl): return not is_menu(gurl)
    def get_menus(gurls): return list(filter(is_menu, gurls))
    def get_files(gurls): return list(filter(not_menu, gurls))

    debug(rootgurl, config)
    content = retrieve_menu_content(rootgurl)
    gurls = getlinks(content, config)
    gurls = list(filter(gurl_ok_config, gurls))

    menus = get_menus(gurls)
    files = get_files(gurls)
    depth = 0

    for menu in menus:
        if depth > config.maxdepth:
            debug("Maxdepth {} reached".format(config.maxdepth), config)
            return list(files)

        debug(menu, config)
        content = retrieve_menu_content(menu)
        all_menu_links = getlinks(content, config)
        new_gurls = list(filter(gurl_ok_config, all_menu_links))
        new_menus = filter_duplicates(get_menus(new_gurls), menus)
        new_files = filter_duplicates(get_files(new_gurls), files)
        menus += new_menus
        files += new_files
        depth += 1
        print("[{} files, {} menus] {}".format(len(files), len(menus), menu.to_url_path()))

    return list(files)

def main():

    optlist, args = ([], [])

    try:
        optlist, args = getopt(argv[1:], Config.getopt_spec)
    except GetoptError:
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
            def probably_a_menu(path):
                # probably a menu if there's no file extension or ends in /
                end = path.split("/")[-1]
                return not "." in end or path[-1] == '/'

            host, port, path = spliturl(host)
            root_gurl_type = "1" if probably_a_menu(path) else "0"
            rootgurl = GopherURL(root_gurl_type, "[ROOT URL]", path, host, port)
            debug("rootgurl: {}".format(rootgurl), config)

            if config.recursive:
                print(":: Downloading menu tree")
                gopher_urls = crawl(rootgurl, config)

                if not config.only_menu and len(gopher_urls) > 0:
                    print(":: Downloading {} files".format(len(gopher_urls)))
                    for i in range(len(gopher_urls)):
                        gurl = gopher_urls[i]
                        print("[{}/{}] {}".format((i + 1), len(gopher_urls), gurl.to_file_path()))
                        write_gopherurl(gurl, config)

            else: 
                # Single file download
                print(":: Downloading single file ")
                print(rootgurl.to_file_path())
                write_gopherurl(rootgurl, config)

        except ValueError as e:
            print(e)


if __name__ == "__main__":
    main()
