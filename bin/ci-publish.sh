#!/usr/bin/env bash
set -eu
SECURE_VAR=${TRAVIS_SECURE_ENV_VARS:-false}
SBT_COMMAND=$1

if [[ "$TRAVIS_SECURE_ENV_VARS" == true ]]; then
  git log | head -n 20
  echo "$PGP_SECRET" | base64 --decode | gpg --import
  if [ -n "$TRAVIS_TAG" ]; then
    sbt $SBT_COMMAND
  else
    echo "Skipping publish, not a tag push"
  fi
else
  echo "Skipping publish, branch=$TRAVIS_BRANCH publish=$PUBLISH test=$CI_TEST"
fi
