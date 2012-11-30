#!/usr/bin/env bash

# build one standalone and one toolbox WAR and run tests
set -e

bash ./scripts/build.sh

echo -e "\nBuilding WAR archives\n"

cd ../CarneadesWebService
lein ring uberwar

cd ../PolicyModellingTool
lein with-profile war ring uberwar
