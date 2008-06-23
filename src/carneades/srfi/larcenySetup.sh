#!/bin/sh
## "larcenySetup.sh"

# The file name convention used here is <lib>.<implementation>.sls
#
# E.g. sharing.larceny.sls, sharing.ikarus.sls, ...
#
# Larceny does NOT use this convention, so we link (on *nix) 
# as follows:

ln -s sharing.larceny.sls    sharing.sls
ln -s parameters.larceny.sls parameters.sls
cd format
ln -s compat.larceny.sls compat.sls
cd ../string-ports
ln -s compat.larceny.sls compat.sls
cd ..
# You probably do a file copy on Windows.

# Don't forget to "chmod +x larcenySetup.sh"
echo "Don't forget to cd <larceny>/lib ; ln -s <srfi>/carneades"
echo "Look at \"tests/print-ascii.ss\""
# -- E O F --
