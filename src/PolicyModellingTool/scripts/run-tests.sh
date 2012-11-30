#!/bin/bash

echo -e "\nRunning tests\n"

lein sub test
lein test
lein cljsbuild test questions
