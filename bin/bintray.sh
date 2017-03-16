#!/usr/bin/env bash

if [[ "$DRONE_BRANCH" == "master" && "$TEST" == "ci-fast" ]]; then
  echo "Running publish from $(pwd)"
  git log | head -n 20
  mkdir -p ~/.bintray
  cat > ~/.bintray/.credentials <<EOF
realm = Bintray API Realm
host = api.bintray.com
user = $BINTRAY_USERNAME
password = $BINTRAY_API_KEY
EOF
  sbt ci-publish
else
  echo "Skipping publish, branch=$DRONE_BRANCH test=$TEST"
fi


