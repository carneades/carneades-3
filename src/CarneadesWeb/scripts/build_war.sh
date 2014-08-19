#!/bin/bash

## vars
WAR_NAME="carneades"

SCRIPTPATH=$( cd $(dirname $0) ; pwd -P )

## Helpers

prepare_local_deps () {

    cd $SCRIPTPATH/../../CarneadesEngine
    lein install
}

build_webclient () {
    cd $SCRIPTPATH/../client
    npm install
    bower install
    grunt build
    cd -
}

show_usage () {
    echo "`basename $0` [--deploy PATH]"
    exit 0
}

## Main
cd $SCRIPTPATH/../server
lein clean

prepare_local_deps
build_webclient

cd $SCRIPTPATH/../server
lein with-profile deploy ring uberwar $WAR_NAME.war

if [[ "$1" == "--deploy" ]]
then
    if [ -z "$2" ]; then
        show_usage
    fi

    cp $SCRIPTPATH/../server/target/$WAR_NAME.war $2
fi
