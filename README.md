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
-f, --forgive <FILE>       the allowlist (default: forgive.yml); a missing file forgives nothing
    --allow <NAME,...>     the axioms every declaration may use; replaces the default
                           propext,Classical.choice,Quot.sound
    --forbid <NAME,...>    names no allowlist entry may forgive, e.g. sorryAx
    --import <MODULE,...>  the modules to load instead of the roots
    --json <FILE>          write the JSON report here; without it, none is written
```

The command line is [lean4-cli](https://github.com/leanprover/lean4-cli)'s, so a flag that takes
several values takes them comma-separated (`--allow propext,Quot.sound`), a value may be attached
with `=`, and `forgive -h`, `forgive audit -h`, and `forgive lint -h` print the help.

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

## The JSON report

`--json <FILE>` writes the whole result as JSON: the violations, the allowlist problems, the
declarations forgiven, and `debt`, the library's own unproved statements ranked by how many
audited declarations reach them. Rendering it is the caller's business.

## Tests

`test/` is a small package that requires this one by path, so the executable is exercised the way
a user runs it. `test/run.sh` builds the fixture library and checks the output and the exit code
of every subcommand and flag.

## As a library

The executable is a thin driver over the `Forgive` library, so a project that wants its own
reporting can call the audit directly: fill in a `Forgive.Config`, read the allowlist with
`Forgive.Forgiveness.read`, run `Forgive.run`, and render the `Forgive.Report` however it likes.
