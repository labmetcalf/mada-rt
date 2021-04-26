#!/bin/bash

cd Documents/Projects/to_archive/mada_rt

git checkout master

# Updated data
Rscript R/get_data.R

# Running on cluster
sub -t 1 -n 9 -jn rt -wt 1m -sn -@ -sp R/rt_epiNow.R

# Stage files to be committed
git add latest

# Commit with date
git commit -m "`date`"

# Push to remote
git push
