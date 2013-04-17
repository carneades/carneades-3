#!/usr/bin/env bash

# build a zip with a self executable JAR

set -e

bash ./scripts/build.sh


PROJECTS_DIR=`grep projects-directory ~/.carneades.clj | cut -d " " -f 3 | sed 's/"//g;' `

lein with-profile standalone uberjar

cd target

rm -rf carneades-webapp

rm -f carneades-webapp.zip

mkdir carneades-webapp

cd carneades-webapp

cp -r ../../../../projects ./projects

rm -f projects/default/databases/*
rm -f projects/copyright/databases/*

cp $PROJECTS_DIR/copyright/databases/main.h2.db ./projects/copyright/databases/

mkdir doc
mkdir config

cp ../../../../doc/manual/out/manual.pdf ./doc/

cp ../carneades-webapp-1.0.0-SNAPSHOT-standalone.jar ./carneades-webapp.jar

cp ../../packaging/README.txt ./

cp ../../packaging/carneades.clj ./config/

echo "This build was created the: " >> ./doc/timestamp.txt
date  >> ./doc/timestamp.txt
echo "The Git Id of this release is: " >> ./doc/timestamp.txt
git log | head -1 >> ./doc/timestamp.txt

cd ..

zip -r carneades-webapp.zip carneades-webapp
