#!/bin/bash

SCRIPTPATH=$( cd $(dirname $0) ; pwd -P )

cd $SCRIPTPATH/../server
echo "Start lein repl. In repl execute (start) to start the server at localhost:3000"

lein repl
