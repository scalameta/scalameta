#!/usr/bin/env bash
set -eux

version=$1

coursier fetch org.scalameta:contrib_2.12:$version -r sonatype:releases
coursier fetch org.scalameta:metacp_2.12:$version -r sonatype:releases
coursier fetch org.scalameta:metap_2.12:$version -r sonatype:releases
coursier fetch org.scalameta:metac_2.12:$version -r sonatype:releases
coursier fetch org.scalameta:semanticdb-scalac_2.12.4:$version -r sonatype:releases
coursier fetch org.scalameta:semanticdb-scalac-core_2.12.4:$version -r sonatype:releases

coursier fetch org.scalameta:contrib_sjs0.6_2.12:$version -r sonatype:releases

coursier fetch org.scalameta:contrib_2.11:$version -r sonatype:releases
coursier fetch org.scalameta:metacp_2.11:$version -r sonatype:releases
coursier fetch org.scalameta:metap_2.11:$version -r sonatype:releases
coursier fetch org.scalameta:metac_2.11:$version -r sonatype:releases
coursier fetch org.scalameta:semanticdb-scalac_2.11.12:$version -r sonatype:releases
coursier fetch org.scalameta:semanticdb-scalac-core_2.11.12:$version -r sonatype:releases

coursier fetch org.scalameta:contrib_native0.3_2.11:$version -r sonatype:releases
coursier fetch org.scalameta:contrib_sjs0.6_2.11:$version -r sonatype:releases
coursier fetch org.scalameta:metap_native0.3_2.11:$version -r sonatype:releases
