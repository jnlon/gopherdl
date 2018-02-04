# gopherdl

*WIP Rewrite in Haskell*

`gopherdl` downloads files and menus over the gopher protocol. It is
similar to wget in terms of commandline options and basic features.

## Usage Examples

Download the index page of gopher.floodgap.com:

`./gopherdl gopher.floodgap.com`

Recursively download all files and menus on gopher.floodgap.com:

`./gopherdl -r gopher.floodgap.com`

Recursively download just menus on gopher.floodgap.com:

`./gopherdl -r -m gopher.floodgap.com`

etc

To see a list of options, run

`./gopherdl -h`


## Purpose

Manually traversing every menu on a large gopher server takes forever,
this tool should make content scraping and discovery much easier.
