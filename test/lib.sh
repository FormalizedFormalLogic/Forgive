# Shared helpers for the suites under test/. A suite sources this file, runs its checks from its
# own package directory, and ends with `summary`.
checks=0
fails=0
out=

note() {
  checks=$((checks + 1))
  if [ "$1" = ok ]; then echo "ok   $2"; else echo "FAIL $2"; fails=$((fails + 1)); fi
}

# run <expected exit> <argument>…: run the executable, leaving its combined output in $out.
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

# has <substring>…: each must appear in the last output.
has() {
  local p
  for p in "$@"; do
    if grep -qF -- "$p" <<<"$out"; then
      note ok "output has: $p"
    else
      note no "output lacks: $p"
      sed 's/^/     /' <<<"$out"
    fi
  done
}

# lacks <substring>: must not appear in the last output.
lacks() {
  if grep -qF -- "$1" <<<"$out"; then
    note no "output has: $1"
    sed 's/^/     /' <<<"$out"
  else
    note ok "output lacks: $1"
  fi
}

# jsonEq <expected> <actual>: equal as JSON, whatever the key order and the formatting.
jsonEq() {
  local d
  if d=$(diff -u <(jq -S . "$1") <(jq -S . "$2") 2>&1); then
    note ok "$2 matches $1"
  else
    note no "$2 differs from $1"
    sed 's/^/     /' <<<"$d"
  fi
}

summary() {
  echo
  if [ "$fails" -eq 0 ]; then
    echo "all $checks checks passed"
  else
    echo "$fails of $checks checks failed"
  fi
  exit $((fails > 0))
}
