#!/bin/bash

### Changes the files to reflect a development configuration

sed -i 's/impact.web.routes-war/impact.web.routes-dev/' project.clj
sed -i 's/debug: false/debug: true/' ./resources/policymodellingtool/public/js/app/config.js