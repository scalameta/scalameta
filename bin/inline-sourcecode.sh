#!/usr/bin/env bash
# Downloads sourcecode sources and vendors them into the scala-2.13 source tree
# under scala.meta.internal.sourcecode.
#
# On Scala 2.13, scalameta uses this vendored copy so that
# com.lihaoyi:sourcecode does not appear as a transitive dependency of the
# published artifacts (which conflicts with sourcecode_3 for downstream Scala 3
# users). On Scala 2.11/2.12 the real library is used directly; compat type
# aliases in those source trees forward to it.
#
# Re-run when upgrading sourcecode.
#
# Usage: ./bin/inline-sourcecode.sh <version>

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 <version>" >&2
  exit 1
fi
VERSION="$1"
NEW_PKG="scala.meta.internal.sourcecode"
DEST_DIR="$REPO_ROOT/scalameta/common/shared/src/main/scala-2.13/scala/meta/internal/sourcecode"

TMP=$(mktemp -d)

JAR_URL="https://repo1.maven.org/maven2/com/lihaoyi/sourcecode_2.13/${VERSION}/sourcecode_2.13-${VERSION}-sources.jar"
curl -fsSL "$JAR_URL" -o "$TMP/sources.jar"
(cd "$TMP" && jar xf sources.jar)

mkdir -p "$DEST_DIR"
find "$DEST_DIR" -name "*.scala" -delete

for SRC in "$TMP/sourcecode/"*.scala; do
  FNAME=$(basename "$SRC")
  sed \
    -e "s/sourcecode/$NEW_PKG/g" \
    -e '/^package /a import scala.language.implicitConversions\nimport scala.language.existentials\n' \
    "$SRC" > "$DEST_DIR/$FNAME"
done

echo "Vendored files written to: $DEST_DIR/"
