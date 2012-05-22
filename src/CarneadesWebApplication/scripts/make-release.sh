#!/bin/bash

### Changes the files to reflect a release configuration

perl -i -pe 's/carneades.web.application.routes-dev/carneades.web.application.routes-war/g' project.clj
perl -i -pe 's/debug: true/debug: false/' ./resources/carneadeswebapplication/public/js/app/config.js
perl -i -pe 's/in_uid_toolbox: false/in_uid_toolbox: true/' ./resources/carneadeswebapplication/public/js/app/config.js
cp resources/carneadeswebapplication/public/indexwebapp.html.in_uid_toolbox resources/carneadeswebapplication/public/indexwebapp.html
