#!/usr/bin/env bash
set -eu
PUBLISH=${CI_PUBLISH:-false}
SECURE_VAR=${TRAVIS_SECURE_ENV_VARS:-false}

bintray() {
  sbt ci-publish
}

sonatype() {
  sbt "sonatypeOpen scalameta-$TRAVIS_TAG" ci-publish sonatypeReleaseAll
}

if [ "$SECURE_VAR" == true ]; then
  git log | head -n 20
  echo "$PGP_SECRET" | base64 --decode | gpg --import
  mkdir -p $HOME/.bintray
  cat > $HOME/.bintray/.credentials <<EOF
realm = Bintray API Realm
host = api.bintray.com
user = ${BINTRAY_USERNAME}
password = ${BINTRAY_API_KEY}
EOF
  if [ -n "$TRAVIS_TAG" ]; then
    sonatype
  else
    bintray
  fi
else
  echo "Skipping publish, branch=$TRAVIS_BRANCH publish=$PUBLISH test=$CI_TEST"
fi
