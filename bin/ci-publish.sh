#!/usr/bin/env bash
set -eu
PUBLISH=${CI_PUBLISH:-false}

if [[ "$TRAVIS_BRANCH" == "master" && "$TRAVIS_PULL_REQUEST" = "false" && "$PUBLISH" == "true" ]]; then
  git log | head -n 20
  mkdir -p $HOME/.bintray
  cat > $HOME/.bintray/.credentials <<EOF
realm = Bintray API Realm
host = api.bintray.com
user = ${BINTRAY_USERNAME}
password = ${BINTRAY_API_KEY}
EOF
  sbt ci-publish
else
  echo "Skipping publish, branch=$TRAVIS_BRANCH publish=$PUBLISH test=$CI_TEST"
fi


