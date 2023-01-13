#!/bin/bash

ASTFILENAME=target

python3.10 ./ast2json.py $1 > /tmp/$ASTFILENAME.json
ocaml 