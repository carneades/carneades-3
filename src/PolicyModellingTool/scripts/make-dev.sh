#!/bin/bash

### Changes the files to reflect a development configuration

perl -i -pe 's/impact.web.routes-war/impact.web.routes-dev/g' project.clj
perl -i -pe 's/debug: false/debug: true/' ./resources/policymodellingtool/public/js/app/config.js
perl -i -pe 's/in_uid_toolbox: true/in_uid_toolbox: false/' ./resources/policymodellingtool/public/js/app/config.js
cp resources/policymodellingtool/public/index.html.local resources/policymodellingtool/public/index.html
