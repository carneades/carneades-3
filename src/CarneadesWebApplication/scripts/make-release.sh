#!/bin/bash

### Changes the files to reflect a release configuration

sed -i 's/carneades.web.application.routes-dev/carneades.web.application.routes-war/' project.clj
