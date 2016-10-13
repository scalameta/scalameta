#!/usr/bin/env bash

if [[ "$CI_BRANCH" == "master" || "$CI_BRANCH" == "2.x" ]]; then
  PUBLISH=publish
  mkdir -p ~/.bintray
  cat > ~/.bintray/.credentials <<EOF
realm = Bintray API Realm
host = api.bintray.com
user = $BINTRAY_USERNAME
password = $BINTRAY_API_KEY
EOF
  sbt ++$SCALA_VERSION "$PUBLISH"
fi


