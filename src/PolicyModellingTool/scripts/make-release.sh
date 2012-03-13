#!/bin/bash

### Changes the files to reflect a release configuration

perl -i -pe 's/impact.web.routes-dev/impact.web.routes-war/' project.clj
perl -i -pe 's/debug: true/debug: false/' ./resources/policymodellingtool/public/js/app/config.js
