#!/bin/bash

ROOTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
OUTDIR=$ROOTDIR/continuous
TMPDIR=`mktemp -d`

cd $TMPDIR
git clone https://code.google.com/p/dart-immutable/
cd dart-immutable
pub install
rm -rf $OUTDIR
dartdoc -v --generate-app-cache --link-api --pkg=packages/ --out $OUTDIR lib/persistent.dart
rm -rf $TMPDIR
