#!/bin/bash

ROOT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"

results=`dart_analyzer --work=/tmp $ROOT_DIR/lib/*.dart 2>&1`
if [ -n "$results" ]; then
    echo "$results"
    exit 1
else
    echo "done"
fi

# TODO(polux): re-enable once https://code.google.com/p/dart/issues/detail?id=8710 is fixed
#dart --enable-checked-mode $ROOT_DIR/example/map_example.dart \
#&& dart --enable-checked-mode $ROOT_DIR/example/set_example.dart \
#&& dart $ROOT_DIR/test/map_test.dart --quiet --quickCheckMaxSize=300 --smallCheckDepth=7 \
#&& dart --enable-checked-mode $ROOT_DIR/test/map_test.dart --quiet --quickCheckMaxSize=100 --smallCheckDepth=5 \
#&& dart $ROOT_DIR/test/set_test.dart --quiet --quickCheckMaxSize=200 --smallCheckDepth=10 \
#&& dart --enable-checked-mode $ROOT_DIR/test/set_test.dart --quiet --quickCheckMaxSize=100 --smallCheckDepth=6

dart $ROOT_DIR/example/map_example.dart \
&& dart $ROOT_DIR/example/set_example.dart \
&& dart $ROOT_DIR/test/map_test.dart --quiet --quickCheckMaxSize=300 --smallCheckDepth=7 \
&& dart $ROOT_DIR/test/set_test.dart --quiet --quickCheckMaxSize=200 --smallCheckDepth=10
