#!/usr/bin/env bash
# End-to-end test of the `forgive` command line against the fixture library in this package.
set -uo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

lake build || exit 1
lake exe forgive --version >/dev/null || exit 1   # build the executable outside a checked run

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
checks=0
fails=0

note() {
  checks=$((checks + 1))
  if [ "$1" = ok ]; then echo "ok   $2"; else echo "FAIL $2"; fails=$((fails + 1)); fi
}

# run <expected exit> <argument>…; leaves the combined output in $out.
run() {
  local want=$1
  shift
  out=$(lake exe forgive "$@" 2>&1)
  local got=$?
  if [ "$got" -eq "$want" ]; then
    note ok "exit $got: forgive $*"
  else
    note no "exit $got, expected $want: forgive $*"
    sed 's/^/     /' <<<"$out"
  fi
}

# has <substring>…, each of which the last output must contain.
has() {
  for p in "$@"; do
    if grep -qF -- "$p" <<<"$out"; then
      note ok "output has: $p"
    else
      note no "output lacks: $p"
      sed 's/^/     /' <<<"$out"
    fi
  done
}

# lacks <substring>, which the last output must not contain.
lacks() {
  if grep -qF -- "$1" <<<"$out"; then
    note no "output has: $1"
    sed 's/^/     /' <<<"$out"
  else
    note ok "output lacks: $1"
  fi
}

exists() { [ -f "$1" ] && note ok "wrote $1" || note no "did not write $1"; }

echo "# audit"
run 0 audit TestLib
has "audited 4 declaration(s) under \`TestLib\`" \
    "2 declaration(s) forgiven by forgive.yml" \
    "TestLib.uses_add_comm' → [TestLib.add_comm']" \
    "forgive: ok"

run 1 audit TestLib -f nope.yml
has "2 declaration(s) under \`TestLib\` use disallowed axioms" \
    "allowed: [propext, Classical.choice, Quot.sound]"

run 0 audit TestLib.lean   # a root as a path to its file
has "audited 4 declaration(s)"

echo "# comma-separated flags"
run 1 audit TestLib -f nope.yml --allow propext,Quot.sound
has "TestLib.allowed → [Classical.choice]" "allowed: [propext, Quot.sound]"

run 0 audit TestLib --import TestLib,TestSorry
has "audited 4 declaration(s)"

echo "# sorryAx"
run 0 audit TestSorry -f sorry.yml
has "TestSorry.missing → [sorryAx]"

run 1 audit TestSorry -f sorry.yml --forbid sorryAx
has "forgiving \`sorryAx\` is not allowed"

echo "# the JSON report"
run 0 audit TestLib --json "$tmp/audit.json"
exists "$tmp/audit.json"
has "report written to $tmp/audit.json"
grep -qF '"audited": 4' "$tmp/audit.json" && note ok "the report counts the audit" \
  || note no "the report does not count the audit"

run 0 audit TestLib
lacks "report written to"

echo "# lint"
run 0 lint -f forgive.yml
has "ok (2 entry(s))"
run 0 lint -f nope.yml
has "nothing to check"
run 1 lint -f sorry.yml --forbid sorryAx
printf 'version: v9\n' > "$tmp/bad.yml"
run 1 lint -f "$tmp/bad.yml"
has "unsupported version \`v9\`"

echo "# usage"
run 2 audit                      # no root module
run 2 audit TestLib --nope       # unknown flag
run 2 lint TestLib               # lint takes no root module
run 2                            # no command
run 0 --help
has "SUBCOMMANDS"
run 0 audit --help
has "--json"
run 0 --version

echo
if [ "$fails" -eq 0 ]; then
  echo "all $checks checks passed"
else
  echo "$fails of $checks checks failed"
fi
exit $((fails > 0))
