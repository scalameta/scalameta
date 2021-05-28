#!/usr/bin/env bash
set -eux

version=$1

coursier resolve \
  org.scalameta:scalameta_2.13:$version \
  org.scalameta:scalameta_2.12:$version \
  org.scalameta:scalameta_2.11:$version \
  org.scalameta:scalameta_sjs1_2.13:$version \
  org.scalameta:scalameta_sjs1_2.12:$version \
  org.scalameta:metac_2.13.6:$version \
  org.scalameta:metac_2.12.14:$version \
  org.scalameta:metac_2.11.12:$version \
  org.scalameta:semanticdb-scalac-core_2.13.6:$version \
  org.scalameta:semanticdb-scalac-core_2.13.5:$version \
  org.scalameta:semanticdb-scalac-core_2.13.4:$version \
  org.scalameta:semanticdb-scalac-core_2.13.3:$version \
  org.scalameta:semanticdb-scalac-core_2.13.2:$version \
  org.scalameta:semanticdb-scalac-core_2.13.1:$version \
  org.scalameta:semanticdb-scalac-core_2.12.13:$version \
  org.scalameta:semanticdb-scalac-core_2.11.12:$version \
  org.scalameta:semanticdb-scalac_2.13.6:$version \
  org.scalameta:semanticdb-scalac_2.13.5:$version \
  org.scalameta:semanticdb-scalac_2.13.4:$version \
  org.scalameta:semanticdb-scalac_2.13.3:$version \
  org.scalameta:semanticdb-scalac_2.13.2:$version \
  org.scalameta:semanticdb-scalac_2.13.1:$version \
  org.scalameta:semanticdb-scalac_2.12.13:$version \
  org.scalameta:semanticdb-scalac_2.11.12:$version \
  -r sonatype:staging
