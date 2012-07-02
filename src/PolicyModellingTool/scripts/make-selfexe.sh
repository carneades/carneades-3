#!/bin/bash

### Changes the files to reflect a self-executable JAR configuration

perl -i -pe 's/impact.web.routes-[^}]+}/impact.web.routes-selfexe}/' project.clj
perl -i -pe 's/debug: [a-z]+/debug: false/' ./resources/policymodellingtool/public/js/app/config.js



