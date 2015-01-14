## Helpers

prepare_local_deps () {

    cd $SCRIPTPATH/../../CarneadesEngine
    lein install
}

build_webclient () {
    cd $SCRIPTPATH/../client
    npm install
    ./node_modules/bower/bin/bower install
    ./node_modules/grunt-cli/bin/grunt deploy
    cd -
}

show_usage () {
    echo "`basename $0` [--deploy PATH]"
    exit 0
}
