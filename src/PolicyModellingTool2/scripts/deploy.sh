#!/bin/bash

TMPDIR="/tmp/"
URL="https://content.wuala.com/contents/policy-impact/WP4%20Policy%20Modelling/Deployment/policymodellingtool2-0.1.0-standalone.war/?key=cGERyGgwHdzN&amp;dl=1"
NAME="policymodellingtool2-0.1.0-standalone.war"
JETTY_HOME=/opt/jetty

wget -r -p $TMPDIR $URL -O $TMPDIR$NAME

ORIGINAL_SUM=`sha1sum $JETTY_HOME/webapps/$NAME | awk '{print $1}'`
CURRENT_SUM=`sha1sum $TMPDIR/$NAME | awk '{print $1}'`

if [ "$CURRENT_SUM" != "$ORIGINAL_SUM" ]
then
    cp $TMPDIR$NAME $JETTY_HOME/webapps/$NAME
    echo "$NAME has been updated";
fi
