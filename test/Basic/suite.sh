#!/usr/bin/env bash
# The executable runs against a fixture library, and its report matches expected.json.
set -uo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"
source ../lib.sh

lake build || exit 1
lake exe forgive --version >/dev/null || exit 1   # build it outside a checked run

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

echo "# it runs"
run 0 TestLib
has "audited 4 declaration(s) under \`TestLib\`" \
    "2 declaration(s) forgiven by forgive.yml" \
    "TestLib.uses_add_comm' → [TestLib.add_comm']" \
    "forgive: ok"

echo "# the JSON report"
run 0 TestLib --json "$tmp/audit.json"
has "report written to $tmp/audit.json"
jsonEq expected.json "$tmp/audit.json"

run 0 TestLib
lacks "report written to"

echo "# usage"
run 2                 # no root module
run 2 TestLib --nope  # unknown flag
run 0 --help
has "USAGE" "--json"

summary
