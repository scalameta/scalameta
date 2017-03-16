#!/usr/bin/env bash

if [[ "$DRONE_BRANCH" == "master" && "$TEST" == "ci-fast" ]]; then
  mkdir -p ~/.bintray
  cat > ~/.bintray/.credentials <<EOF
realm = Bintray API Realm
host = api.bintray.com
user = $BINTRAY_USERNAME
password = $BINTRAY_API_KEY
EOF
  sbt "very publish"
fi


