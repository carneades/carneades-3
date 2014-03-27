#!/bin/bash

## vars
WAR_NAME="carneades"

SCRIPTPATH=$( cd $(dirname $0) ; pwd -P )

## Helpers

prepare_local_deps () {

    cd $SCRIPTPATH/../../CarneadesEngine
    lein install

    cd $SCRIPTPATH/../../CarneadesWebService
    lein install
}

show_usage () {
    echo "`basename $0` [--deploy PATH]"
    exit 0
}

## Main


prepare_local_deps

cd $SCRIPTPATH/../server
lein with-profiles war ring uberwar $WAR_NAME.war

if [[ "$1" == "--deploy" ]]
then
    if [ -z "$2" ]; then
        show_usage
    fi

    mv $SCRIPTPATH/../server/target/$WAR_NAME.war $2
fi
