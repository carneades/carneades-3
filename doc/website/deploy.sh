#!/bin/bash

echo -e "Deploying updates to Github..."

# Build the project. 
hugo

# Change to the toplevel directory of the working Github tree

cd ../..
 
# Add changes to git.
git add -A

# Commit changes.
msg="rebuilding site `date`"
if [ $# -eq 1 ]
  then msg="$1"
fi
git commit -m "$msg"

# Push source and build repos.
git push origin master
git subtree push --prefix=doc/website/public git@github.com:carneades/carneades.git gh-pages


















