#!/usr/bin/env bash

# build a zip with a self executable JAR

set -e

bash ./scripts/build.sh


DATABASE_DIR=`grep database-host=  ~/.carneades.properties | cut -d = -f 2`

lein with-profile standalone uberjar

cd target

rm -rf carneades-webapp

rm -f carneades-webapp.zip

mkdir carneades-webapp

cd carneades-webapp

mkdir -p config data/databases doc policies

cp $DATABASE_DIR/copyright.h2.db ./data/databases

cp ../../../../doc/manual/out/manual.pdf ./doc/

cp ../../../CarneadesExamples/src/carneades/examples/policies.clj ./policies

POLICIES=`grep ":filename " ../../../CarneadesExamples/src/carneades/examples/policies.clj | tr -s " " | cut -d " " -f 3 | sed s'/\}//g'`

for policy in $POLICIES; do
    cp ../../../CarneadesExamples/src/carneades/examples/$policy ./policies/$policy
done

cp ../policymodellingtool-1.0.0-SNAPSHOT-standalone.jar ./carneades-webapp.jar

cp ../../packaging/README.txt ./

cp ../../packaging/carneades.properties ./config

echo "This build was created the: " >> ./doc/timestamp.txt
date  >> ./doc/timestamp.txt
echo "The Git Id of this release is: " >> ./doc/timestamp.txt
git log | head -1 >> ./doc/timestamp.txt

cd ..

zip -r carneades-webapp.zip carneades-webapp
