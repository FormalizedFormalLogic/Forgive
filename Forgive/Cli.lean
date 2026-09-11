import Cli
import Forgive.Render

/-!
# The command line
-/

open Lean Cli

namespace Forgive.Cli

/-- A declaration or axiom name, such as `Classical.choice` or `sorryAx`. -/
def DeclName := Lean.Name
  deriving Inhabited, BEq, Repr, ToString

instance : ParseableType DeclName where
  name     := "Name"
  parse? s :=
    let n := s.toName
    if n.isAnonymous then none else some n

private def declName (n : DeclName) : Name := n

private def moduleName (m : ModuleName) : Name := m

private def names (p : Parsed) (flag : String) : Option (List Name) :=
  p.flag? flag |>.map fun f => (f.as! (Array DeclName)).toList.map declName

/-- The flags `audit` and `lint` share. -/
private def commonConfig (p : Parsed) : Config :=
  let cfg : Config := { forgiveFile := p.flag! "forgive" |>.as! String }
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
    jsonFile? := (p.flag? "json").map fun f => (f.as! String : System.FilePath) }

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

/-- Load the roots and audit them. -/
def runAudit (cfg : Config) : IO UInt32 := do
  let fg ← match ← Forgiveness.read cfg.forgiveFile with
    | .ok fg => pure fg
    | .error msg =>
      writeReport cfg (errorJson cfg msg)
      IO.eprintln s!"forgive: {msg}"
      return 1
  let result ← try
      pure (Except.ok (← Forgive.run cfg fg))
    catch e => pure (Except.error (toString e))
  let r ← match result with
    | .ok r => pure r
    | .error msg =>
      let msg := s!"failed to load the environment: {msg}"
      writeReport cfg (errorJson cfg msg)
      IO.eprintln s!"forgive: {msg}"
      return 2
  writeReport cfg (r.toJson cfg)
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
  if let some file := cfg.jsonFile? then
    IO.println s!"forgive: report written to {file}"
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

def auditCmd : Cmd := `[Cli|
  audit VIA runAuditCmd;
  "Audit every declaration defined under <ROOT>... against the allowlist. It reads the oleans, \
   so run it through lake after a build: `lake exe forgive audit MyLib`."

  FLAGS:
    f, forgive : String;         "The allowlist; a missing file forgives nothing."
    allow : Array DeclName;      "The axioms every declaration may use, replacing the default \
                                  `propext,Classical.choice,Quot.sound`."
    forbid : Array DeclName;     "Names no allowlist entry may forgive, e.g. `sorryAx`."
    "import" : Array ModuleName; "The modules to load instead of the roots."
    json : String;               "Write the JSON report here."

  ARGS:
    ...roots : ModuleName;       "A root module; its submodules are audited too."

  EXTENSIONS:
    defaultValues! #[("forgive", ({} : Config).forgiveFile.toString)]
]

def lintCmd : Cmd := `[Cli|
  lint VIA runLintCmd;
  "Check the allowlist alone: that it parses, and that it forgives nothing --forbid disallows. \
   It loads no environment, so it needs no build."

  FLAGS:
    f, forgive : String;    "The allowlist; a missing file forgives nothing."
    forbid : Array DeclName; "Names no allowlist entry may forgive, e.g. `sorryAx`."

  EXTENSIONS:
    defaultValues! #[("forgive", ({} : Config).forgiveFile.toString)]
]

def forgiveCmd : Cmd := `[Cli|
  forgive NOOP; ["0.1.0"]
  "Audit the axioms a Lean library uses, against an allowlist. Exits 0 when clean, 1 on \
   violations or problems in the allowlist, and 2 on bad usage or an environment that failed \
   to load."

  SUBCOMMANDS:
    auditCmd;
    lintCmd
]

/-- The entry point; unlike `Cmd.validate`, bad usage exits 2. -/
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
