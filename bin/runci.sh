#!/bin/sh
set -eux
TEST=${1}

case "$TEST" in
  "check" )
    ./bin/checkCLA.sh
    ./bin/scalafmt --test
    ;;
  * )
    sbt $TEST
    ;;
esac

