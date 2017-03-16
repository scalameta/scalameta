#!/usr/bin/env bash
username=$1
api_key=$2

if [[ "$DRONE_BRANCH" == "master" && "$TEST" == "ci-fast" ]]; then
  echo "Running publish from $(pwd)"
  git log | head -n 20
  mkdir -p ~/.bintray
  cat > ~/.bintray/.credentials <<EOF
realm = Bintray API Realm
host = api.bintray.com
user = $username
password = $api_key
EOF
  sbt ci-publish
else
  echo "Skipping publish, branch=$DRONE_BRANCH test=$TEST"
fi


