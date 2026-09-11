import Cli
import Forgive

/-!
# The command line
-/

open Lean Cli

namespace Forgive.Cli

private def moduleName (m : ModuleName) : Name := m

private def config (p : Parsed) : Config :=
  { forgiveFile := p.flag! "forgive" |>.as! String
    roots := (p.variableArgsAs! ModuleName).map moduleName
    imports := match p.flag? "import" with
      | some f => (f.as! (Array ModuleName)).map moduleName
      | none => #[]
    jsonFile? := (p.flag? "json").map fun f => (f.as! String : System.FilePath) }

/-- Load the roots and audit them. -/
def runAudit (cfg : Config) : IO UInt32 := do
  let fg ← match ← Forgiveness.read cfg.forgiveFile with
    | .ok fg => pure fg
    | .error msg =>
      writeReport cfg (errorJson cfg msg)
      IO.eprintln s!"forgive: {msg}"
      return 1
  let cfg := cfg.accepting fg
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

private def runForgive (p : Parsed) : IO UInt32 := do
  let cfg := config p
  if cfg.roots.isEmpty then
    IO.eprintln "forgive: expected at least one root module"
    return 2
  runAudit cfg

def forgiveCmd : Cmd := `[Cli|
  forgive VIA runForgive; ["0.1.0"]
  "Audit the axioms every declaration defined under <ROOT>... uses, against an allowlist. It \
   reads the oleans, so run it through lake after a build: `lake exe forgive MyLib`. Exits 0 \
   when clean, 1 on violations or problems in the allowlist, and 2 on bad usage or an \
   environment that failed to load."

  FLAGS:
    f, forgive : String;         "The allowlist; a missing file forgives nothing."
    "import" : Array ModuleName; "The modules to load instead of the roots."
    json : String;               "Write the JSON report here."

  ARGS:
    ...roots : ModuleName;       "A root module; its submodules are audited too."

  EXTENSIONS:
    defaultValues! #[("forgive", ({} : Config).forgiveFile.toString)]
]

/-- The entry point; unlike `Cmd.validate`, bad usage exits 2. -/
def main (args : List String) : IO UInt32 := do
  match forgiveCmd.process args with
  | .ok (cmd, p) =>
    if p.hasFlag "help" then
      p.printHelp
      return 0
    if p.hasFlag "version" then
      p.printVersion!
      return 0
    cmd.run p
  | .error (cmd, err) =>
    cmd.printError err
    return 2

end Forgive.Cli
