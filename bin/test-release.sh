#!/bin/sh
set -eux
VERSION=${1}
coursier fetch org.scalameta:scalameta_2.12:$VERSION -r sonatype:releases
coursier fetch org.scalameta:scalameta_2.11:$VERSION -r sonatype:releases
coursier fetch org.scalameta:langmeta_2.10:$VERSION -r sonatype:releases
coursier fetch org.scalameta:semanticdb-scalac_2.12.4:$VERSION -r sonatype:releases
coursier fetch org.scalameta:semanticdb-scalac_2.11.12:$VERSION -r sonatype:releases
