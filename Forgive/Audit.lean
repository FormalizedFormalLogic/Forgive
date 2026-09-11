import Forgive.Allowlist
import Forgive.Axioms
import Forgive.Config

/-!
# The audit

Every declaration under an audited root is checked against the allowed axioms; an allowlist entry
is honoured only when cutting the names it forgives leaves nothing disallowed behind, and every
entry and every forgiven name must earn its place.
-/

open Lean

namespace Forgive

/-- The result of an axiom audit. -/
structure Report where
  audited : Nat
  /-- Distinct axioms used anywhere under the roots, sorted. -/
  axiomsUsed : Array String
  /-- `(axiom, how many other audited declarations reach it)` for each axiom outside the allowed
  ones: the library's own unproved statements, most depended-on first. -/
  debt : Array (String × Nat)
  /-- `(declaration, the disallowed axioms it uses)` for each offending declaration. For a
  declaration with an allowlist entry, the axioms are those the entry does not forgive. -/
  violations : Array (String × Array String)
  /-- `(declaration, the disallowed axioms it uses)` for each declaration the allowlist forgives. -/
  forgiven : Array (String × Array String)
  /-- Problems with the allowlist: entries naming unknown or clean declarations, items forgiving
  nothing, names the configuration forbids. -/
  forgiveErrors : Array String

/-- Whether the audit has no violations or allowlist errors. -/
def Report.ok (r : Report) : Bool := r.violations.isEmpty && r.forgiveErrors.isEmpty

/-- The allowlist problems that need no environment: the forbidden names it forgives. Reported
both by the audit and by the standalone allowlist check. -/
def forbiddenErrors (cfg : Config) (fg : Forgiveness) : Array String :=
  (fg.forbiddenUses cfg.forbidden).map fun (d, x) => s!"`{d}`: forgiving `{x}` is not allowed"

/-- Intermediate data used to construct an audit report. -/
private structure Analysis where
  usedAll : NameSet := {}
  /-- How many audited declarations, other than the axiom itself, reach each disallowed axiom. -/
  debt : NameMap Nat := {}
  violations : Array (Name × Array Name) := #[]
  forgiven : Array (Name × Array Name) := #[]
  errors : Array String := #[]

/-- The disallowed axioms `d` still reaches when the names in `cut` are forgiven. -/
private def remaining (allowed cut : NameSet) (d : Name) : AxiomM (Array Name) := do
  let axs ← (axiomsOfCut allowed cut d).run' {}
  return axs.filter (!allowed.contains ·)

private def analyze (cfg : Config) (allowed : NameSet) (candidates : Array Name)
    (fg : Forgiveness) : AxiomM Analysis := do
  let env ← read
  let mut r : Analysis := { errors := forbiddenErrors cfg fg }
  let err (r : Analysis) (msg : String) : Analysis := { r with errors := r.errors.push msg }
  let candSet : NameSet := candidates.foldl (·.insert ·) {}
  for e in fg.entries do
    if !env.contains e.decl then
      r := err r s!"`{freshStr e.decl}` is not a declaration"
    else if !candSet.contains e.decl then
      r := err r s!"`{freshStr e.decl}` is not defined under {cfg.rootsStr}"
    for x in e.forgive do
      if !env.contains x then
        r := err r s!"`{freshStr e.decl}`: `{freshStr x}` is neither a declaration nor an axiom"
  for d in candidates do
    let axs ← axiomsOf d
    r := { r with usedAll := axs.foldl (·.insert ·) r.usedAll }
    let bad := axs.filter (!allowed.contains ·)
    -- An audited declaration that is itself a disallowed axiom is one of the library's own
    -- unproved statements: it belongs in the table even when nothing depends on it yet.
    if isAxiom env d && !allowed.contains d then
      r := { r with debt := r.debt.insert d ((r.debt.find? d).getD 0) }
    for a in bad do
      if a != d then r := { r with debt := r.debt.insert a ((r.debt.find? a).getD 0 + 1) }
    match fg.find? d with
    | none =>
      if !bad.isEmpty then r := { r with violations := r.violations.push (d, bad) }
    | some e =>
      if bad.isEmpty then
        r := err r s!"`{freshStr d}` uses no disallowed axiom; remove its entry"
        continue
      let cut : NameSet := e.forgive.foldl (·.insert ·) {}
      let left ← remaining allowed cut d
      if !left.isEmpty then
        r := { r with violations := r.violations.push (d, left) }
        continue
      r := { r with forgiven := r.forgiven.push (d, bad) }
      for x in e.forgive do
        if env.contains x && (← remaining allowed (cut.erase x) d).isEmpty then
          r := err r s!"`{freshStr d}`: `{freshStr x}` is redundant: the other forgiven names already cover it"
  return r

/-- Audit every declaration defined under `cfg.roots`, against `cfg.allowed` and the allowlist. -/
def audit (cfg : Config) (fg : Forgiveness) : CoreM Report := do
  let env ← getEnv
  let allowedSet : NameSet := cfg.allowed.foldl (·.insert ·) {}
  let modNames := env.allImportedModuleNames
  -- Candidates: declarations defined in a module under a root.
  let candidates : Array Name := env.constants.fold (init := #[]) fun acc declName _ =>
    match env.getModuleIdxFor? declName with
    | some idx =>
      match modNames[idx.toNat]? with
      | some m => if cfg.covers m then acc.push declName else acc
      | none => acc
    | none => acc
  let candidates := candidates.qsort Name.lt
  let a := ((analyze cfg allowedSet candidates fg).run env).run' {}
  let render (p : Name × Array Name) : String × Array String := (freshStr p.1, p.2.map freshStr)
  return {
    audited := candidates.size
    axiomsUsed := (a.usedAll.toArray.qsort Name.lt).map freshStr
    debt := (a.debt.toList.toArray.qsort fun x y =>
      x.2 > y.2 || (x.2 == y.2 && Name.lt x.1 y.1)).map fun (n, c) => (freshStr n, c)
    violations := a.violations.map render
    forgiven := a.forgiven.map render
    forgiveErrors := a.errors.map fun s => s.foldl (fun acc c => acc.push c) ""
  }

/-- Run `act` in the environment built from the given imported modules. -/
def withImportedEnv {α} (modules : Array Name) (act : CoreM α) : IO α := do
  initSearchPath (← findSysroot)
  unsafe Lean.withImportModules (modules.map (fun m => { module := m })) {} (trustLevel := 1024)
    fun env => Prod.fst <$> Core.CoreM.toIO act
      (ctx := { fileName := "<forgive>", fileMap := default }) (s := { env := env })

/-- Load `cfg.modules` and audit them. -/
def run (cfg : Config) (fg : Forgiveness) : IO Report :=
  withImportedEnv cfg.modules (audit cfg fg)

end Forgive
