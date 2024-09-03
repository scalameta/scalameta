#!/usr/bin/env bash
set -eux

version=$1

sbt \
  "set every version := \"$version\"" \
  ++2.12.7 semanticdbShared/publishSigned semanticdbScalacPlugin/publishSigned semanticdbScalacCore/publishSigned  \
  sonatypeReleaseAll
