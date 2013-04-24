#!/bin/bash

ROOT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"

DART_VERSION=`dart --version 2>&1`
GIT_HASH=`git rev-parse HEAD`
DATE=`date -u`
SCORE=`dart $ROOT_DIR/test/map_bench_wordcount.dart`

echo "$DART_VERSION, $GIT_HASH, $DATE, $SCORE" >> $ROOT_DIR/test/map_bench_wordcount.csv

