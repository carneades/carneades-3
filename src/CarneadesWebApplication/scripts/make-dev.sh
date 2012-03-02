#!/bin/bash

### Changes the files to reflect a development configuration

sed -i 's/carneades.web.application.routes-war/carneades.web.application.routes-dev/' project.clj