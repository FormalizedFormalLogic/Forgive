# Forgive

An axiom audit for Lean 4 libraries. It reports the axioms every declaration under a root module
transitively uses and fails on any axiom outside a small allowed set — except where an allowlist
file, `forgive.yml`, forgives it by name.

The point of the allowlist is that an unproved statement need not be a `sorry`. A `sorry`
collapses every unproved result into `sorryAx`, so the audit can only say that something is
missing. Declared instead as an `axiom` under the name its theorem will keep, and forgiven under
that name, the audit reports exactly which unproved statements a declaration leans on; proving it
later turns the `axiom` into a `theorem` and deletes the entry.

## Use

Require the package, and build the library before auditing it — the audit reads the oleans.

```toml
# lakefile.toml
[[require]]
name = "Forgive"
git = "https://github.com/FormalizedFormalLogic/Forgive"
rev = "<commit>"
```

```bash
lake build
lake exe forgive audit MyLib
```

Run it through `lake`, which puts the built library on `LEAN_PATH`. Several roots are allowed
(`lake exe forgive audit MyLib MyLibExtras`); a declaration is audited when the module defining it
is a root or one of its submodules.

```
-f, --forgive <FILE>   the allowlist (default: forgive.yml); a missing file forgives nothing
    --allow <NAME>     an axiom every declaration may use. Repeatable; the first use
                       replaces the default propext, Classical.choice, Quot.sound
    --forbid <NAME>    a name no allowlist entry may forgive, e.g. sorryAx. Repeatable
    --import <MODULE>  a module to load instead of the roots. Repeatable
    --json <FILE>      where the JSON report goes (default: .lake/audit.json)
    --markdown <FILE>  where the Markdown report goes (default: .lake/audit.md)
    --no-json          write no JSON report
    --no-markdown      write no Markdown report
    --title <TEXT>     the heading of the Markdown report (default: Axiom audit)
```

It exits `0` when clean, `1` on violations or problems in the allowlist, and `2` on bad usage or
an environment that failed to load.

## The allowlist

```yaml
version: v0

MyLib.some_unproved_lemma:
  forgive:
    - MyLib.some_unproved_lemma
MyLib.uses_some_unproved_lemma:
  forgive:
    - MyLib.some_unproved_lemma
```

Each top-level declaration key lists the axioms or declarations through which its dependencies may
be forgiven: the declaration passes when cutting those names out of the dependency graph leaves no
disallowed axiom behind. Forgiving a name therefore forgives the whole subtree under it, which is
why an entry that forgives its own name cuts everything the declaration rests on.

Entries are checked, not trusted. The audit reports an entry whose declaration does not exist or
is not under a root, an entry for a declaration that uses no disallowed axiom, a forgiven name
that is neither a declaration nor an axiom, and a forgiven name the other names in the same entry
already cover.

Only the subset of YAML shown above is understood: block mappings, block sequences, flow sequences
of scalars (`forgive: [a, b]`), one layer of quoting, and `#` comments.

## `forgive lint`

`forgive lint` checks the allowlist alone — that it parses, and that it forgives nothing
`--forbid` disallows. It loads no environment, so it runs without a build:

```bash
lake exe forgive lint --forbid sorryAx
```

## The reports

`audit` writes a machine-readable `.lake/audit.json` and a `.lake/audit.md` meant to be posted as
a single pull-request comment. Besides the violations and the allowlist problems, the Markdown
report ranks the library's own unproved statements by how many audited declarations reach them.
The forgiven table is capped at 150 rows, since GitHub rejects a comment over 65536 characters.

## As a library

The executable is a thin driver over the `Forgive` library, so a project that wants its own
reporting can call the audit directly: fill in a `Forgive.Config`, read the allowlist with
`Forgive.Forgiveness.read`, run `Forgive.run`, and render the `Forgive.Report` however it likes.
