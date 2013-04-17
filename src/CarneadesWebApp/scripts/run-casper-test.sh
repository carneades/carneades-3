#!/usr/bin/env bash

COOKIE=/tmp/casperjs-cookie.txt

rm -f $COOKIE /tmp/carneades.png

casperjs --url=http://localhost:8080/carneades/policy-analysis/#/introduction --cookies-file=$COOKIE casper/run-scenario-test.js
