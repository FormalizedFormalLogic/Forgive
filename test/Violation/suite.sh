#!/usr/bin/env bash
# An axiom the allowlist does not permit must be reported, and must fail the run.
set -uo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"
source ../lib.sh

lake build || exit 1
lake exe forgive --version >/dev/null || exit 1   # build it outside a checked run

run 1 TestLib
has "2 declaration(s) under \`TestLib\` use disallowed axioms" \
    "TestLib.add_comm' → [TestLib.add_comm']" \
    "TestLib.uses_add_comm' → [TestLib.add_comm']" \
    "allowed: [propext, Quot.sound, Classical.choice]"
lacks "forgive: ok"

summary
