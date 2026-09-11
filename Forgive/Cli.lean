import Forgive.Render

/-!
# The command line

`forgive audit <ROOT>…` loads the roots and audits them; `forgive lint` checks the allowlist
alone and loads no environment, so it runs without a build.
-/

open Lean

namespace Forgive.Cli

/-- What the executable was asked to do. -/
inductive Command where
  | audit
  | lint
  deriving Repr, DecidableEq

def usage : String := r#"forgive — audit the axioms a Lean library uses, against an allowlist

USAGE
  forgive audit [OPTIONS] <ROOT>...   audit every declaration defined under <ROOT>
  forgive lint [OPTIONS]              check the allowlist alone; loads no environment

OPTIONS
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
  -h, --help             this message

Run it through lake (`lake exe forgive audit MyLib`), which puts the built library on LEAN_PATH;
`audit` reads the oleans, so the library has to have been built.

EXIT STATUS
  0  clean
  1  violations, problems in the allowlist, or an allowlist that does not parse
  2  bad usage, or the environment failed to load
"#

private structure Parsed where
  cfg : Config := {}
  /-- Whether `--allow` has been given: the first one replaces the default axioms. -/
  allowSeen : Bool := false

private def withCfg (p : Parsed) (f : Config → Config) : Parsed := { p with cfg := f p.cfg }

private def toName (what s : String) : Except String Name :=
  let n := s.toName
  if n.isAnonymous then throw s!"{what}: `{s}` is not a name" else return n

/-- The options that take a value, so that a trailing one gets a better message than "unknown". -/
private def valueOptions : List String :=
  ["-f", "--forgive", "--allow", "--forbid", "--import", "--json", "--markdown", "--title"]

private partial def parseOpts (p : Parsed) : List String → Except String Parsed
  | [] => return p
  | "-f" :: v :: rest | "--forgive" :: v :: rest =>
    parseOpts (withCfg p fun c => { c with forgiveFile := v }) rest
  | "--json" :: v :: rest => parseOpts (withCfg p fun c => { c with jsonFile? := some v }) rest
  | "--markdown" :: v :: rest =>
    parseOpts (withCfg p fun c => { c with markdownFile? := some v }) rest
  | "--no-json" :: rest => parseOpts (withCfg p fun c => { c with jsonFile? := none }) rest
  | "--no-markdown" :: rest => parseOpts (withCfg p fun c => { c with markdownFile? := none }) rest
  | "--title" :: v :: rest => parseOpts (withCfg p fun c => { c with title := v }) rest
  | "--allow" :: v :: rest => do
    let n ← toName "--allow" v
    let p := if p.allowSeen then p else withCfg p fun c => { c with allowed := [] }
    let p := withCfg p fun c => { c with allowed := c.allowed ++ [n] }
    parseOpts { p with allowSeen := true } rest
  | "--forbid" :: v :: rest => do
    let n ← toName "--forbid" v
    parseOpts (withCfg p fun c => { c with forbidden := c.forbidden ++ [n] }) rest
  | "--import" :: v :: rest => do
    let n ← toName "--import" v
    parseOpts (withCfg p fun c => { c with imports := c.imports.push n }) rest
  | a :: rest => do
    if a.startsWith "-" then
      if valueOptions.contains a then throw s!"`{a}` needs a value"
      throw s!"unknown option `{a}`"
    let n ← toName "root" a
    parseOpts (withCfg p fun c => { c with roots := c.roots.push n }) rest

/-- Read a command line. -/
def parse : List String → Except String (Command × Config)
  | [] => throw "expected a command: `audit` or `lint`"
  | cmdArg :: rest => do
    let cmd ← match cmdArg with
      | "audit" => pure Command.audit
      | "lint" => pure Command.lint
      | a => throw s!"unknown command `{a}` (expected `audit` or `lint`)"
    let cfg := (← parseOpts {} rest).cfg
    match cmd with
    | .audit => if cfg.roots.isEmpty then throw "`audit` needs at least one root module"
    | .lint => if !cfg.roots.isEmpty then throw "`lint` takes no root modules"
    return (cmd, cfg)

/-! ## The commands -/

/-- Check the allowlist alone: that it parses, and that it forgives nothing forbidden. -/
def runLint (cfg : Config) : IO UInt32 := do
  if !(← cfg.forgiveFile.pathExists) then
    IO.println s!"forgive: no allowlist at {cfg.forgiveFile}; nothing to check"
    return 0
  let fg ← match ← Forgiveness.read cfg.forgiveFile with
    | .ok fg => pure fg
    | .error msg =>
      IO.eprintln s!"forgive: {msg}"
      return 1
  let errs := forbiddenErrors cfg fg
  if errs.isEmpty then
    IO.println s!"forgive: {cfg.forgiveFile}: ok ({fg.entries.size} entry(s))"
    return 0
  IO.eprintln s!"forgive: {errs.size} problem(s) in {cfg.forgiveFile}:"
  for e in errs do IO.eprintln s!"  {e}"
  return 1

/-- Load the roots and audit them, writing the reports the configuration asks for. -/
def runAudit (cfg : Config) : IO UInt32 := do
  let fg ← match ← Forgiveness.read cfg.forgiveFile with
    | .ok fg => pure fg
    | .error msg =>
      writeReports cfg (errorJson cfg msg) (errorMarkdown cfg msg)
      IO.eprintln s!"forgive: {msg}"
      return 1
  let result ← try
      pure (Except.ok (← Forgive.run cfg fg))
    catch e => pure (Except.error (toString e))
  let r ← match result with
    | .ok r => pure r
    | .error msg =>
      let msg := s!"failed to load the environment: {msg}"
      writeReports cfg (errorJson cfg msg) (errorMarkdown cfg msg)
      IO.eprintln s!"forgive: {msg}"
      return 2
  writeReports cfg (r.toJson cfg) (r.toMarkdown cfg)
  IO.println s!"forgive: audited {r.audited} declaration(s) under {cfg.rootsStr}; \
    axioms used: {r.axiomsUsed.toList}"
  (← IO.getStdout).flush
  if !r.forgiven.isEmpty then
    IO.println s!"forgive: {r.forgiven.size} declaration(s) forgiven by {cfg.forgiveFile}:"
    for (d, axs) in r.forgiven do
      IO.println s!"  {d} → {axs.toList}"
    (← IO.getStdout).flush
  if !r.forgiveErrors.isEmpty then
    IO.eprintln s!"forgive: {r.forgiveErrors.size} problem(s) in {cfg.forgiveFile}:"
    for e in r.forgiveErrors do
      IO.eprintln s!"  {e}"
  if !r.violations.isEmpty then
    IO.eprintln s!"forgive: {r.violations.size} declaration(s) under {cfg.rootsStr} \
      use disallowed axioms:"
    for (d, axs) in r.violations do
      IO.eprintln s!"  {d} → {axs.toList}"
    IO.eprintln s!"allowed: {cfg.allowed}"
  let paths := reportPaths cfg
  if !paths.isEmpty then
    IO.println s!"forgive: reports written to {" and ".intercalate paths.toList}"
  if r.ok then
    IO.println "forgive: ok"
    return 0
  return 1

/-- The entry point. -/
def main (args : List String) : IO UInt32 := do
  if args.contains "-h" || args.contains "--help" then
    IO.println usage
    return 0
  match parse args with
  | .error e =>
    IO.eprintln s!"forgive: {e}"
    IO.eprintln ""
    IO.eprint usage
    return 2
  | .ok (.audit, cfg) => runAudit cfg
  | .ok (.lint, cfg) => runLint cfg

end Forgive.Cli
