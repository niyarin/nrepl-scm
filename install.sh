#!/bin/sh
set -e

PREFIX="${PREFIX:-/usr/local}"
SHARE="$PREFIX/share/nrepl-scm"
BIN="$PREFIX/bin"

echo "Installing to $PREFIX ..."

mkdir -p "$SHARE/nrepl"
cp boot.scm "$SHARE/"
cp nrepl/*.scm "$SHARE/nrepl/"

mkdir -p "$BIN"
sed "s|NREPL_PATH=\./|NREPL_PATH=$SHARE|" nrepl-scm > "$BIN/nrepl-scm"
chmod +x "$BIN/nrepl-scm"

echo "Done."
echo "  libraries -> $SHARE"
echo "  command   -> $BIN/nrepl-scm"
