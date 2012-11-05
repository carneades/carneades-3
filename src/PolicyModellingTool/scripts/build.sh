#!/usr/bin/env bash

set -e

lein sub clean
lein sub install

echo -e "\nBuild is finished. \n"
echo -e "To run the Aston example:\n$ cd ../CarneadesExamples && lein run -m carneades.examples.aston\n"
echo -e "To run the Policy Modelling tool:\n$ lein ring server 8080"
echo -e "Then point your browser at http://localhost:8080/policymodellingtool/#/introduction"
