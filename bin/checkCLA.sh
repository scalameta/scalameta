#!/usr/bin/env bash
set -eux

AUTHOR=$GITHUB_ACTOR
echo "Pull request submitted by $AUTHOR";
url_author=$(echo $AUTHOR | jq -Rr "@uri")
signed=$(curl -s https://www.lightbend.com/contribute/cla/scala/check/$url_author | jq -r ".signed")
if [ "$signed" = "true" ] ; then
  echo "CLA check for $AUTHOR successful";
else
  echo "CLA check for $AUTHOR failed";
  echo "Please sign the Scala CLA to contribute to Scalameta";
  echo "Go to https://www.lightbend.com/contribute/cla/scala and then resubmit this pull request";
  exit 1;
fi;
