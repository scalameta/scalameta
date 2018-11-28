#!/usr/bin/env bash
set -eux

version=$1

sbt \
  "set every version := \"$version\"" \
  ++2.12.6 semanticdbScalacPlugin/publishSigned \
  ++2.12.5 semanticdbScalacPlugin/publishSigned \
  ++2.12.4 semanticdbScalacPlugin/publishSigned
  # Stop at 2.12.3 because it doesn't compile due to
  # missing `OriginalAttachment` added in 2.12.4.

# Restart sbt to avoid OOM
sbt \
  "set every version := \"$version\"" \
  ++2.11.11 semanticdbScalacPlugin/publishSigned \
  ++2.11.10 semanticdbScalacPlugin/publishSigned \
  ++2.11.9 semanticdbScalacPlugin/publishSigned
  # Stop at 2.11.8 because it's more than 2 year old.
