#!/bin/bash

ROOT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"

dartanalyzer $ROOT_DIR/lib/*.dart \
&& dartanalyzer $ROOT_DIR/test/*.dart \
&& dartanalyzer $ROOT_DIR/benchmark/*.dart \
&& dartanalyzer $ROOT_DIR/example/*.dart \
&& dart --enable-checked-mode $ROOT_DIR/example/map_example.dart \
&& dart --enable-checked-mode $ROOT_DIR/example/set_example.dart \
&& dart --enable-checked-mode $ROOT_DIR/test/option_test.dart \
&& dart $ROOT_DIR/test/map_test.dart --quiet --quickCheckMaxSize=300 --smallCheckDepth=7 \
&& dart --enable-checked-mode $ROOT_DIR/test/map_test.dart --quiet --quickCheckMaxSize=100 --smallCheckDepth=5 \
&& dart $ROOT_DIR/test/set_test.dart --quiet --quickCheckMaxSize=200 --smallCheckDepth=10 \
&& dart --enable-checked-mode $ROOT_DIR/test/set_test.dart --quiet --quickCheckMaxSize=100 --smallCheckDepth=6
