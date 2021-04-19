#!/bin/bash

cd Documents/Projects/mada-rt

git checkout master

# Updated data
Rscript R/get_data.R

# Run locally (takes abt an hour)
Rscript R/rt_epiNow.R

# Stage files to be committed
git add latest

# Commit with date
git commit -m "`date`"

# Push to remote
git push
