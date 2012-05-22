#!/bin/bash

### Changes the files to reflect a development configuration

perl -i -pe 's/carneades.web.application.routes-war/carneades.web.application.routes-dev/g' project.clj
perl -i -pe 's/debug: false/debug: true/' ./resources/carneadeswebapplication/public/js/app/config.js
perl -i -pe 's/in_uid_toolbox: true/in_uid_toolbox: false/' ./resources/carneadeswebapplication/public/js/app/config.js
cp resources/carneadeswebapplication/public/indexwebapp.html.local resources/carneadeswebapplication/public/indexwebapp.html
