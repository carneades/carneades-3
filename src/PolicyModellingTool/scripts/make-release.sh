#!/bin/bash

### Changes the files to reflect a release configuration

sed -i 's/impact.web.routes-dev/impact.web.routes-war/' project.clj
sed -i 's/debug: true/debug: false/' ./resources/public/js/app/config.js
