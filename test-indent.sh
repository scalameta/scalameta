#!/bin/bash

set -e

bloop test testsJVM -o scala.meta.tests.parsers.dotty.SignificantIndentationSuite
bloop test testsJVM -o scala.meta.tests.parsers.dotty.ControlSyntaxSuite
bloop test testsJVM -o scala.meta.tests.parsers.dotty.MinorDottySuite

bloop test dottytests
