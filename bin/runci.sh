#!/bin/sh
set -eux
TEST=${1}

case "$TEST" in
  "check" )
    ./bin/scalafmt --test
    ;;
  * )
    sbt $TEST
    ;;
esac

