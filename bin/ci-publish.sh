#!/usr/bin/env bash
set -eu
PUBLISH=${CI_PUBLISH:-false}
SECURE_VAR=${TRAVIS_SECURE_ENV_VARS:-false}

if [[ "$TRAVIS_SECURE_ENV_VARS" == true && "$CI_PUBLISH" == true ]]; then
  git log | head -n 20
  echo "$PGP_SECRET" | base64 --decode | gpg --import
  if [ -n "$TRAVIS_TAG" ]; then
    sbt ci-publish
  else
    sbt -Dscalameta.snapshot=true ci-publish
  fi
else
  echo "Skipping publish, branch=$TRAVIS_BRANCH publish=$PUBLISH test=$CI_TEST"
fi
