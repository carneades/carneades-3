#!/bin/bash

### Changes the files to reflect a development configuration

perl -i -pe 's/impact.web.routes-[a-z]+/impact.web.routes-dev/' project.clj
perl -i -pe 's/debug: [a-z]+/debug: true/' ./resources/policymodellingtool/public/js/app/config.js


