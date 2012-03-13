#!/bin/bash

### Changes the files to reflect a development configuration

perl -i -pe 's/carneades.web.application.routes-war/carneades.web.application.routes-dev/g' project.clj