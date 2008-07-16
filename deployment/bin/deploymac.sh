#!/bin/bash

cd dist/
zip -r carneades-mac-$1.zip Carneades/ -x */lib/Carneades.jar */Carneades.bat */deploy* 


