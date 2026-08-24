#!/usr/bin/env bash
set -eux

version=$1
scala=$2

# semanticdb has a row per Scala patch, and releaseSemanticdbFor names the rows of one patch
sbt \
  "set every version := \"$version\"" \
  semanticdbShared2_13/publishSigned \
  "releaseSemanticdbFor $scala" \
  sonatypeReleaseAll
