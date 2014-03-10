#!/bin/bash

#### vars
WAR_NAME="carneades"
#TOMCAT_DEPLOY_PATH="/var/lib/tomcat8/webapps"
TOMCAT_DEPLOY_PATH="/usr/local/Cellar/tomcat/7.0.52/libexec/webapps"

#################################################################################
####                                                                            #
#### HELPERS                                                                    #
####                                                                            #
####                                                                            #
#################################################################################
set_deploy_path () {
    cont=true
    while $cont
    do
        printf "Set the path to your tomcat webapps folder (${TOMCAT_DEPLOY_PATH}): "
        read line
        case "$line" in
            '')
                cont=false
                ;;
            *)
                if [ -d "$line" ];
                then
                    TOMCAT_DEPLOY_PATH=$line
                    cont=false
                else
                    echo "Folder \'$line\' does not exist. try again."
                fi
                ;;

        esac
    done
}

prepare_local_deps () {
    SCRIPTPATH=$( cd $(dirname $0) ; pwd -P )

    cd $SCRIPTPATH/../../CarneadesEngine
    lein install

    cd $SCRIPTPATH/../../CarneadesWebService
    lein install
}

prepare_local_deps
cd $SCRIPTPATH/../server
lein with-profiles tomcat ring uberwar $WAR_NAME.war

if [[ $* == *--deploy* ]]
then
    set_deploy_path
    mv $SCRIPTPATH/../server/target/$WAR_NAME.war $TOMCAT_DEPLOY_PATH
fi
