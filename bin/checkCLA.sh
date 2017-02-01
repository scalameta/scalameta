#!/usr/bin/env bash
set -e

if [ -n "$CI_PULL_REQUEST" ] ; then
  echo "Incoming pull request from https://github.com/$CI_REPO/pull/$CI_PULL_REQUEST";
  author=$(curl -s "https://api.github.com/repos/$CI_REPO/pulls/$CI_PULL_REQUEST" | jq -r ".user.login");
  if [ $? -ne 0 ] ; then exit 1; fi;
  echo "Pull request submitted by $author";
  signed=$(curl -s http://www.lightbend.com/contribute/cla/scala/check/$author | jq -r ".signed");
  if [ $? -ne 0 ] ; then exit 1; fi;
  if [ "$signed" = "true" ] ; then
    echo "CLA check for $author successful";
  else
    echo "CLA check for $author failed";
    echo "Please sign the Scala CLA to contribute to scala.meta";
    echo "Go to https://www.lightbend.com/contribute/cla/scala and then resubmit this pull request";
    exit 1;
  fi;
fi

