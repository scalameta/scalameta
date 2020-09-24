#!/bin/bash

set -e

git clone https://github.com/lampepfl/dotty.git dotty-codebase
cd dotty-codebase
# commit hash from 24.09 14:40
git checkout 85d1322c4e8a7254b67dd7b88fa8fdf87b4dac72
cd ..
