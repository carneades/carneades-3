#!/usr/bin/env bash

# build a zip with a self executable JAR

set -e

WAR_NAME="carneades-webapp"

SCRIPTPATH=$( cd $(dirname $0) ; pwd -P )

PROJECTS_DIR=`grep :projects-directory ~/.carneades.clj | sed -n 's/^.*"\(.*\)"$/\1/p'`

source $SCRIPTPATH/helpers.sh

cd $SCRIPTPATH/../../CarneadesExamples
lein deps
lein run -m carneades.examples.copyright-arguments &> /dev/null && true

cd $SCRIPTPATH/../server

lein clean

prepare_local_deps
build_webclient

cd $SCRIPTPATH/../server
lein with-profile jar ring uberjar

cd target

rm -rf carneades-webapp

rm -f carneades-webapp.zip

mkdir carneades-webapp

cd carneades-webapp

cp -r ../../../../../projects ./projects

rm -f projects/default/databases/*
rm -f projects/copyright/databases/*

mkdir -p ./projects/copyright/databases/
mkdir -p ./projects/markos/databases/

cp $PROJECTS_DIR/copyright/databases/main.h2.db ./projects/copyright/databases/
cp $PROJECTS_DIR/markos/databases/legal-profiles.h2.db ./projects/markos/databases/

mkdir doc
mkdir config

cp ../../../../../doc/manual/out/manual.pdf ./doc/

cp ../carneades-web-1.0.0-SNAPSHOT-standalone.jar ./carneades-webapp.jar

cp ../../../scripts/packaging/README.txt ./

cp ../../../scripts/packaging/carneades.clj ./config/

echo "This build was created the: " >> ./doc/timestamp.txt
date  >> ./doc/timestamp.txt
echo "The Git id of this release is: " >> ./doc/timestamp.txt
git log | head -1 >> ./doc/timestamp.txt

cd ..

zip -r carneades-webapp.zip carneades-webapp

if [ -x /usr/bin/notify-send ]; then
    notify-send "Build package" "Build package is finished"
fi


