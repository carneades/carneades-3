#!/bin/bash

### Changes the files to reflect a release configuration which includes the toolbox css

perl -i -pe 's/impact.web.routes-[a-z]+/impact.web.routes-war/' project.clj
perl -i -pe 's/debug: [a-z]+/debug: false/' ./resources/policymodellingtool/public/js/app/config.js


