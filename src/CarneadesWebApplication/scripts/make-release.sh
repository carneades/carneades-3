#!/bin/bash

### Changes the files to reflect a release configuration

perl -i -pe 's/carneades.web.application.routes-dev/carneades.web.application.routes-war/g' project.clj
perl -i -pe 's/debug: true/debug: false/' ./resources/carneadeswebapplication/public/js/app/config.js