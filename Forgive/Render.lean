import Forgive.Audit

/-!
# Rendering

Two renderings of the same report: JSON for a machine, and Markdown for the pull-request comment.
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

private def code (s : String) : String := s!"`{s}`"

private def codes (xs : Array String) : String :=
  if xs.isEmpty then "none" else ", ".intercalate (xs.map code).toList

private def declTable (rows : Array (String × Array String)) : String :=
  "| Declaration | Disallowed axioms |\n|---|---|\n"
    ++ "".intercalate (rows.map fun (d, axs) => s!"| {code d} | {codes axs} |\n").toList

/-- The report as Markdown, for the pull-request comment. -/
def Report.toMarkdown (cfg : Config) (r : Report) : String := Id.run do
  let forgiveFile := cfg.forgiveFile.toString
  let status :=
    if r.ok then "✅ clean"
    else ", ".intercalate <| List.filter (!·.isEmpty) [
      if r.violations.isEmpty then "" else s!"❌ {r.violations.size} violation(s)",
      if r.forgiveErrors.isEmpty then ""
        else s!"❌ {r.forgiveErrors.size} problem(s) in {code forgiveFile}"]
  let mut md := s!"## {cfg.title}\n\n| | |\n|---|---|\n"
  md := md ++ s!"| **Status** | {status} |\n"
  md := md ++ s!"| **Audited** | {r.audited} declaration(s) under {cfg.rootsStr} |\n"
  md := md ++ s!"| **Allowed axioms** | {codes (cfg.allowed.map toString).toArray} |\n"
  md := md ++ s!"| **Unproved statements** | {r.debt.size} |\n"
  if !r.violations.isEmpty then
    md := md ++ s!"\n### Violations ({r.violations.size})\n\n" ++ declTable r.violations
  if !r.forgiveErrors.isEmpty then
    md := md ++ s!"\n### Problems in {code forgiveFile} ({r.forgiveErrors.size})\n\n"
      ++ "".intercalate (r.forgiveErrors.map (s!"- {·}\n")).toList
  if !r.debt.isEmpty then
    md := md ++ s!"\n### Unproved statements ({r.debt.size})\n\n"
      ++ "The axioms this library declares in place of a proof, most depended-on first. "
      ++ "The count is how many other audited declarations reach the axiom, so it ranks the "
      ++ "statements by how much of the library rests on them.\n\n"
      ++ "| Statement | Dependent declarations |\n|---|---|\n"
      ++ "".intercalate (r.debt.map fun (d, c) => s!"| {code d} | {c} |\n").toList
  if !r.forgiven.isEmpty then
    -- Capped: the report is posted as one pull-request comment, and GitHub rejects a comment
    -- over 65536 characters. The uncapped list is the allowlist file itself.
    let shown := r.forgiven.take 150
    md := md ++ s!"\n<details><summary>Forgiven by {code forgiveFile}"
    md := md ++ s!" ({r.forgiven.size} declaration(s))</summary>\n\n" ++ declTable shown
    if shown.size < r.forgiven.size then
      md := md ++ s!"\n… and {r.forgiven.size - shown.size} more;"
      md := md ++ s!" the full list is {code forgiveFile}.\n"
    md := md ++ "\n</details>\n"
  return md

def errorMarkdown (cfg : Config) (msg : String) : String :=
  s!"## {cfg.title}\n\n| | |\n|---|---|\n| **Status** | ❌ did not run |\n\n```\n{msg}\n```\n"

/-- Write whichever of the two reports the configuration asks for. -/
def writeReports (cfg : Config) (json : Json) (md : String) : IO Unit := do
  if let some file := cfg.jsonFile? then
    if let some dir := file.parent then IO.FS.createDirAll dir
    IO.FS.writeFile file (json.pretty ++ "\n")
  if let some file := cfg.markdownFile? then
    if let some dir := file.parent then IO.FS.createDirAll dir
    IO.FS.writeFile file md

/-- Where the reports went, for the closing line of the run. -/
def reportPaths (cfg : Config) : Array String :=
  #[cfg.jsonFile?, cfg.markdownFile?].filterMap (·.map System.FilePath.toString)

end Forgive
