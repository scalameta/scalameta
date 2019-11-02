#!/usr/bin/env bash
set -eux

version=$1

coursier fetch \
  org.scalameta:scalameta_2.13:$version \
  org.scalameta:scalameta_2.12:$version \
  org.scalameta:scalameta_2.11:$version \
  org.scalameta:scalameta_sjs0.6_2.13:$version \
  org.scalameta:scalameta_sjs0.6_2.12:$version \
  org.scalameta:scalameta_sjs0.6_2.11:$version \
  org.scalameta:metac_2.13.1:$version \
  org.scalameta:metac_2.12.10:$version \
  org.scalameta:metac_2.11.12:$version \
  org.scalameta:semanticdb-scalac-core_2.13.1:$version \
  org.scalameta:semanticdb-scalac-core_2.12.10:$version \
  org.scalameta:semanticdb-scalac-core_2.11.12:$version \
  org.scalameta:semanticdb-scalac_2.13.1:$version \
  org.scalameta:semanticdb-scalac_2.12.10:$version \
  org.scalameta:semanticdb-scalac_2.11.12:$version \
  -r sonatype:public

# Scala Native is disabled until v0.4
  # org.scalameta:scalameta_native0.3_2.11:$version \

# These artifacts were published manually, they are not published by the CI.
# These artfacts will be dropped in the next release.
coursier fetch \
  org.scalameta:semanticdb-scalac-core_2.13.0:$version \
  org.scalameta:semanticdb-scalac-core_2.12.9:$version \
  org.scalameta:semanticdb-scalac-core_2.12.8:$version \
  org.scalameta:semanticdb-scalac_2.13.1:$version \
  org.scalameta:semanticdb-scalac_2.12.9:$version \
  org.scalameta:semanticdb-scalac_2.12.8:$version \
  -r sonatype:public
