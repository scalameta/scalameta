#!/usr/bin/env bash
set -eux

version=$1
scala=$2

# sonaRelease uploads everything in it
rm -rf target/sona-staging
# semanticdb has a row per Scala patch, and releaseSemanticdbFor names the rows of one patch
sbt --server --batch -Dbackport.release=1 \
  "set every version := \"$version\"; releaseSemanticdbFor $scala; sonaRelease"
