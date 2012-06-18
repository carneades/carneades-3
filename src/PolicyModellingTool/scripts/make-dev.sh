#!/bin/bash

### Changes the files to reflect a development configuration

perl -i -pe 's/impact.web.routes-[a-z]+/impact.web.routes-dev/' project.clj
perl -i -pe 's/debug: [a-z]+/debug: true/' ./resources/policymodellingtool/public/js/app/config.js
perl -i -pe 's/in_uid_toolbox: [a-z]+/in_uid_toolbox: false/' ./resources/policymodellingtool/public/js/app/config.js 
cp resources/policymodellingtool/public/index.html.local resources/policymodellingtool/public/index.html
