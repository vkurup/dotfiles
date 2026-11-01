#!/usr/bin/env bash
# Run my Emacs regression tests against the real init.el.
#   test/run-tests.sh                  run everything
#   test/run-tests.sh beancount/       run tests whose names match a regexp
set -euo pipefail

test_dir="$(cd "$(dirname "$0")" && pwd)"
selector="${1:-}"

loads=()
for f in "$test_dir"/*-test.el; do
  loads+=(-l "$f")
done

# Run from /tmp so nothing gets picked up from the current directory.
cd /tmp
exec emacs --batch \
  -L "$test_dir" \
  -l "$test_dir/test-helper.el" \
  "${loads[@]}" \
  --eval "(ert-run-tests-batch-and-exit \"$selector\")"
