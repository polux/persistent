#!/bin/bash

ROOT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"

DART_VERSION=`dart --version 2>&1`
GIT_HASH=`git rev-parse HEAD`
DATE=`date -u`
SCORE=`dart $ROOT_DIR/benchmark/map_bench_wordcount.dart`

echo "\"$( echo $DART_VERSION | sed "s/\"/\"\"/g" )\", $GIT_HASH, $DATE, $SCORE" >> $ROOT_DIR/benchmark/map_bench_wordcount.csv

