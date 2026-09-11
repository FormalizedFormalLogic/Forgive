import Cli
import Forgive.Render

/-!
# The command line

`forgive audit <ROOT>…` loads the roots and audits them; `forgive lint` checks the allowlist
alone and loads no environment, so it runs without a build.
-/

open Lean Cli

namespace Forgive.Cli

/-! ## Argument types -/

/-- A declaration or axiom name on the command line, such as `Classical.choice` or `sorryAx`. -/
def DeclName := Lean.Name
  deriving Inhabited, BEq, Repr, ToString

instance : ParseableType DeclName where
  name     := "Name"
  parse? s :=
    let n := s.toName
    if n.isAnonymous then none else some n

private def declName (n : DeclName) : Name := n

private def moduleName (m : ModuleName) : Name := m

/-! ## From a parsed command line to a `Config` -/

/-- The defaults, so that the help and `Config` cannot drift apart. -/
private def dflt : Config := {}

private def names (p : Parsed) (flag : String) : Option (List Name) :=
  p.flag? flag |>.map fun f => (f.as! (Array DeclName)).toList.map declName

private def path (p : Parsed) (flag : String) : System.FilePath :=
  p.flag! flag |>.as! String

/-- The flags `audit` and `lint` share. -/
private def commonConfig (p : Parsed) : Config :=
  let cfg : Config := { forgiveFile := path p "forgive" }
  let cfg := match names p "allow" with
    | some ns => { cfg with allowed := ns }
    | none => cfg
  match names p "forbid" with
  | some ns => { cfg with forbidden := ns }
  | none => cfg

private def auditConfig (p : Parsed) : Config :=
  { commonConfig p with
    roots := (p.variableArgsAs! ModuleName).map moduleName
    imports := match p.flag? "import" with
      | some f => (f.as! (Array ModuleName)).map moduleName
      | none => #[]
    jsonFile? := if p.hasFlag "no-json" then none else some (path p "json")
    markdownFile? := if p.hasFlag "no-markdown" then none else some (path p "markdown")
    title := p.flag! "title" |>.as! String }

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

private def runAuditCmd (p : Parsed) : IO UInt32 := do
  let cfg := auditConfig p
  if cfg.roots.isEmpty then
    IO.eprintln "forgive: `audit` needs at least one root module"
    return 2
  runAudit cfg

private def runLintCmd (p : Parsed) : IO UInt32 :=
  runLint (commonConfig p)

/-- `forgive audit <ROOT>…`: audit a built library against the allowlist. -/
def auditCmd : Cmd := `[Cli|
  audit VIA runAuditCmd;
  "Audit every declaration defined under <ROOT>... against the allowlist. Run it through lake \
   (`lake exe forgive audit MyLib`), which puts the built library on LEAN_PATH; audit reads the \
   oleans, so the library has to have been built."

  FLAGS:
    f, forgive : String;         "The allowlist; a missing file forgives nothing."
    allow : Array DeclName;          "The axioms every declaration may use, comma-separated. \
                                  Replaces the default `propext,Classical.choice,Quot.sound`."
    forbid : Array DeclName;         "Names no allowlist entry may forgive, comma-separated, \
                                  e.g. `sorryAx`."
    "import" : Array ModuleName; "The modules to load instead of the roots, comma-separated."
    json : String;               "Where the JSON report goes."
    "no-json";                   "Write no JSON report."
    markdown : String;           "Where the Markdown report goes."
    "no-markdown";               "Write no Markdown report."
    title : String;              "The heading of the Markdown report."

  ARGS:
    ...roots : ModuleName;       "A root module. A declaration is audited when the module \
                                  defining it is a root or one of its submodules."

  EXTENSIONS:
    defaultValues! #[
      ("forgive", dflt.forgiveFile.toString),
      ("json", (dflt.jsonFile?.getD "").toString),
      ("markdown", (dflt.markdownFile?.getD "").toString),
      ("title", dflt.title)
    ]
]

/-- `forgive lint`: check the allowlist alone, without loading an environment. -/
def lintCmd : Cmd := `[Cli|
  lint VIA runLintCmd;
  "Check the allowlist alone: that it parses, and that it forgives nothing --forbid disallows. \
   It loads no environment, so it runs without a build."

  FLAGS:
    f, forgive : String; "The allowlist; a missing file forgives nothing."
    forbid : Array DeclName; "Names no allowlist entry may forgive, comma-separated, e.g. `sorryAx`."

  EXTENSIONS:
    defaultValues! #[("forgive", dflt.forgiveFile.toString)]
]

/-- The root command. -/
def forgiveCmd : Cmd := `[Cli|
  forgive NOOP; ["0.1.0"]
  "Audit the axioms a Lean library uses, against an allowlist. Exits 0 when clean, 1 on \
   violations or problems in the allowlist, and 2 on bad usage or an environment that failed \
   to load."

  SUBCOMMANDS:
    auditCmd;
    lintCmd
]

/-- The entry point. Bad usage exits 2, as the `audit` and `lint` failures exit 1. -/
def main (args : List String) : IO UInt32 := do
  match forgiveCmd.process args with
  | .ok (cmd, p) =>
    if p.hasFlag "help" then
      p.printHelp
      return 0
    if p.cmd.meta.hasVersion && p.hasFlag "version" then
      p.printVersion!
      return 0
    if !p.hasParent then
      IO.eprintln "forgive: expected a command: `audit` or `lint`"
      p.printHelp
      return 2
    cmd.run p
  | .error (cmd, err) =>
    cmd.printError err
    return 2

end Forgive.Cli
