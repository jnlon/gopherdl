# gopherdl

`gopherdl` is a Python3 program for downloading files and menus over the gopher
protocol. It is similar to wget in terms of commandline options and basic
features. 

## Usage Examples

Download the index page of gopher.floodgap.com:

`./gopherdl.py gopher.floodgap.com`

Recursively download all files and menus on gopher.floodgap.com:

`./gopherdl.py -r gopher.floodgap.com`

Recursively download just menus on gopher.floodgap.com:

`./gopherdl.py -r -m gopher.floodgap.com`

etc

To see a list of options, run

`./gopherdl.py -h`


## Why

It takes forever to manually traverse every menu on a large gopher server, so
hopefully this tool will make content discovery easier.

And, to my surprise, there doesn't seem to exist a wget-equivalent for the
gopher protocol, how strange! Hopefully gopherdl fills the gap.
