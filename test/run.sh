#!/usr/bin/env bash
# Run every test/<name>/suite.sh.
set -uo pipefail
shopt -s nullglob
cd "$(dirname "${BASH_SOURCE[0]}")"

command -v jq >/dev/null || { echo "test: jq is required"; exit 1; }

failed=()
for suite in */suite.sh; do
  name=${suite%/suite.sh}
  echo "### $name"
  bash "$suite" || failed+=("$name")
  echo
done

if [ ${#failed[@]} -eq 0 ]; then
  echo "every suite passed"
else
  echo "suites that failed: ${failed[*]}"
  exit 1
fi
