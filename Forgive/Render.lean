import Forgive.Audit

/-!
# The JSON report
-/

open Lean

namespace Forgive

private def pair (p : String × Array String) : Json :=
  Json.mkObj [("decl", Json.str p.1), ("axioms", Lean.toJson p.2)]

/-- The machine-readable report. -/
def Report.toJson (cfg : Config) (r : Report) : Json :=
  Json.mkObj [
    ("roots", Lean.toJson (cfg.roots.map toString)),
    ("allowed", Lean.toJson (cfg.allowed.map toString)),
    ("forgiveFile", Json.str cfg.forgiveFile.toString),
    ("audited", Lean.toJson r.audited),
    ("ok", Json.bool r.ok),
    ("axiomsUsed", Lean.toJson r.axiomsUsed),
    ("debt", Lean.toJson (r.debt.map fun (d, c) =>
      Json.mkObj [("axiom", Json.str d), ("dependents", Lean.toJson c)])),
    ("violations", Lean.toJson (r.violations.map pair)),
    ("forgiven", Lean.toJson (r.forgiven.map pair)),
    ("forgiveErrors", Lean.toJson r.forgiveErrors)
  ]

/-- A report that never got as far as the environment: the allowlist did not parse, or the
environment did not load. -/
def errorJson (cfg : Config) (msg : String) : Json :=
  Json.mkObj [
    ("roots", Lean.toJson (cfg.roots.map toString)),
    ("ok", Json.bool false),
    ("error", Json.str msg)
  ]

/-- Write the report, if `--json` asked for one. -/
def writeReport (cfg : Config) (json : Json) : IO Unit := do
  let some file := cfg.jsonFile? | return
  if let some dir := file.parent then IO.FS.createDirAll dir
  IO.FS.writeFile file (json.pretty ++ "\n")

end Forgive
