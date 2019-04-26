#!/usr/bin/env bash
set -eux

version=$1

coursier fetch \
  org.scalameta:scalameta_2.12:$version \
  org.scalameta:scalameta_2.11:$version \
  org.scalameta:scalameta_native0.3_2.11:$version \
  org.scalameta:scalameta_sjs0.6_2.12:$version \
  org.scalameta:scalameta_sjs0.6_2.11:$version \
  org.scalameta:metac_2.12.8:$version \
  org.scalameta:metac_2.11.12:$version \
  org.scalameta:semanticdb-scalac-core_2.12.8:$version \
  org.scalameta:semanticdb-scalac-core_2.12.7:$version \
  org.scalameta:semanticdb-scalac-core_2.11.12:$version \
  org.scalameta:semanticdb-scalac_2.12.8:$version \
  org.scalameta:semanticdb-scalac_2.12.7:$version \
  org.scalameta:semanticdb-scalac_2.11.12:$version \
  -r sonatype:public

# These artifacts were published manually, they are not published by the CI.
# These artfacts will be dropped in the next release.
coursier fetch \
  org.scalameta:semanticdb-scalac-core_2.12.6:$version \
  org.scalameta:semanticdb-scalac-core_2.12.5:$version \
  org.scalameta:semanticdb-scalac-core_2.12.4:$version \
  org.scalameta:semanticdb-scalac-core_2.11.11:$version \
  org.scalameta:semanticdb-scalac-core_2.11.10:$version \
  org.scalameta:semanticdb-scalac-core_2.11.9:$version \
  org.scalameta:semanticdb-scalac_2.12.6:$version \
  org.scalameta:semanticdb-scalac_2.12.5:$version \
  org.scalameta:semanticdb-scalac_2.12.4:$version \
  org.scalameta:semanticdb-scalac_2.11.11:$version \
  org.scalameta:semanticdb-scalac_2.11.10:$version \
  org.scalameta:semanticdb-scalac_2.11.9:$version \
  -r sonatype:public
