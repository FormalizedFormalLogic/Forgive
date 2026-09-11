# Forgive

An axiom audit for Lean 4 libraries. Every declaration under a root module must reach only the
axioms `forgive.yml` accepts — except where it forgives a name by hand.

A `sorry` collapses every unproved result into `sorryAx`. Written instead as an `axiom` under the
name its theorem will keep, and forgiven under that name, it appears in the report as itself;
proving it turns the `axiom` into a `theorem` and deletes the entry.

[leanprover-community/axiom-audit](https://github.com/leanprover-community/axiom-audit) is the
same audit with one flat `--allow` list. Use that if a flat list is all you need.

## Use

```toml
# lakefile.toml
[[require]]
name = "Forgive"
git = "https://github.com/FormalizedFormalLogic/Forgive"
rev = "<commit>"
```

```bash
lake build                           # the audit reads the oleans
lake exe forgive MyLib MyLibExtras   # several roots are allowed
```

A declaration is audited when the module defining it is a root or one of its submodules.

```
-f, --forgive <FILE>       the allowlist (default: forgive.yml)
    --import <MODULE,...>  the modules to load instead of the roots
    --json <FILE>          write the JSON report here
```

It exits `0` when clean, `1` on violations or a bad allowlist, and `2` on bad usage or an
environment that failed to load. The command line is
[lean4-cli](https://github.com/leanprover/lean4-cli)'s; `forgive -h` prints the help.

## `forgive.yml`

```yaml
version: v0

accept:
  - propext
  - Quot.sound
  - Classical.choice

declaration:
  MyLib.some_unproved_lemma:
    forgive:
      - MyLib.some_unproved_lemma
  MyLib.uses_some_unproved_lemma:
    forgive:
      - MyLib.some_unproved_lemma
```

`accept` replaces the default `propext`, `Classical.choice`, `Quot.sound`. An entry passes when
cutting the names it forgives out of the dependency graph leaves no disallowed axiom behind, so
forgiving a name forgives the whole subtree under it. A missing file forgives nothing.

Entries are checked, not trusted: an unknown or out-of-root declaration, an entry for a clean one,
a forgiven name that exists nowhere, and a redundant forgiven name are all reported. Only block
mappings and sequences, flow sequences of scalars, one layer of quoting, and `#` comments parse.

`--json <FILE>` writes the same result machine-readably, plus `debt` — the library's own unproved
statements, ranked by how many audited declarations reach them.

## Developing

```bash
just test
```
