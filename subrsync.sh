#!/bin/bash
## user:=mrajeev
## cluster:=della.princeton.edu
## dirput:=bash
## -r recurse through sub-directories
## -L transform symlink into reference file/dir
## -v be verbose about printing messages
## -z compresses data before transfer and decompresses after transfer
## -t pass the timestamp when syncing

rsync -rLvzt --update --exclude '*.git' --exclude '.Rproj*' --exclude 'renv*' --exclude 'cron*' ~/Documents/Projects/to_archive/mada_rt mrajeev@della.princeton.edu:~/
