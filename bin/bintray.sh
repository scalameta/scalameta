#!/usr/bin/env bash
set -eu

if [[ "$DRONE_BRANCH" == "master" && "$CI_PUBLISH" == "true" ]]; then
  echo "Running publish from $(pwd)"
  git log | head -n 20
  mkdir -p $HOME/.bintray
  cat > $HOME/.bintray/.credentials <<EOF
realm = Bintray API Realm
host = api.bintray.com
user = ${BINTRAY_USERNAME}
password = ${BINTRAY_API_KEY}
EOF
  /usr/bin/sbt ci-publish
else
  echo "Skipping publish, branch=$DRONE_BRANCH test=$CI_TEST"
fi


