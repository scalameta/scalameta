#!/usr/bin/env bash
set -eux

version=$1

coursier fetch \
  org.scalameta:contrib_2.12:$version -r sonatype:releases \
  org.scalameta:symtab_2.12:$version -r sonatype:releases \
  org.scalameta:metacp_2.12:$version -r sonatype:releases \
  org.scalameta:metai_2.12:$version -r sonatype:releases \
  org.scalameta:metap_2.12:$version -r sonatype:releases \
  org.scalameta:metac_2.12.7:$version -r sonatype:releases \
  org.scalameta:interactive_2.12.7:$version -r sonatype:releases \
  org.scalameta:semanticdb-javac_2.12:$version -r sonatype:releases \
  org.scalameta:semanticdb-scalac_2.12.7:$version -r sonatype:releases \
  org.scalameta:semanticdb-scalac_2.12.6:$version -r sonatype:releases \
  org.scalameta:semanticdb-scalac_2.12.5:$version -r sonatype:releases \
  org.scalameta:semanticdb-scalac_2.12.4:$version -r sonatype:releases \
  org.scalameta:semanticdb-scalac-core_2.12.7:$version -r sonatype:releases \
  org.scalameta:contrib_sjs0.6_2.12:$version -r sonatype:releases \
  org.scalameta:contrib_2.11:$version -r sonatype:releases \
  org.scalameta:symtab_2.11:$version -r sonatype:releases \
  org.scalameta:metacp_2.11:$version -r sonatype:releases \
  org.scalameta:metai_2.11:$version -r sonatype:releases \
  org.scalameta:metap_2.11:$version -r sonatype:releases \
  org.scalameta:metac_2.11.12:$version -r sonatype:releases \
  org.scalameta:semanticdb-javac_2.11:$version -r sonatype:releases \
  org.scalameta:semanticdb-scalac_2.11.12:$version -r sonatype:releases \
  org.scalameta:semanticdb-scalac_2.11.11:$version -r sonatype:releases \
  org.scalameta:semanticdb-scalac_2.11.10:$version -r sonatype:releases \
  org.scalameta:semanticdb-scalac_2.11.9:$version -r sonatype:releases \
  org.scalameta:semanticdb-scalac-core_2.11.12:$version -r sonatype:releases \
  org.scalameta:contrib_native0.3_2.11:$version -r sonatype:releases \
  org.scalameta:contrib_sjs0.6_2.11:$version -r sonatype:releases \
  org.scalameta:metap_native0.3_2.11:$version -r sonatype:releases
