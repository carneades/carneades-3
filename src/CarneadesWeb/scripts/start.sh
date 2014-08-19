#!/bin/bash

SCRIPTPATH=$( cd $(dirname $0) ; pwd -P )

cd $SCRIPTPATH/../server
echo "Starting the server..."
echo "Once the server is started you can access the"
echo "the web application by typing"
echo "'http://localhost:8081/carneades' into the browser."
echo "###################################################"
echo "#           Carneades web application             #"
echo "###################################################"
lein exec -ep "(use 'user) (go)"
