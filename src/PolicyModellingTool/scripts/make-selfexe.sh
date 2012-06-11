#!/bin/bash

### Changes the files to reflect a self-executable JAR configuration

perl -i -pe 's/impact.web.routes-[^}]+}/impact.web.routes-selfexe}/' project.clj
perl -i -pe 's/debug: true/debug: false/' ./resources/policymodellingtool/public/js/app/config.js
perl -i -pe 's/in_uid_toolbox: false/in_uid_toolbox: true/' ./resources/policymodellingtool/public/js/app/config.js 
cp resources/policymodellingtool/public/index.html.in_uid_toolbox resources/policymodellingtool/public/index.html


