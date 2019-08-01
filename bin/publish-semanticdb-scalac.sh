#!/usr/bin/env bash
set -eux

version=$1

sbt \
  "set every version := \"$version\"" \
  ++2.12.7 semanticdbScalacPlugin/publishSigned semanticdbScalacCore/publishSigned  \
  sonatypeReleaseAll
