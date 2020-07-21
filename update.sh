#!/bin/bash

cd Documents/Projects/mada-rt

# Whichever one you want to be the home page output as index.html
Rscript R/rt_epiNow.R

# Stage files to be committed
git add latest

# Commit with date
git commit -m "`date`"

# Push to remote
git push
