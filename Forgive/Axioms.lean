import Lean

/-!
# Collecting axioms

The transitive axioms of a constant, and the same traversal with a set of names cut out of it:
cutting is what forgiving a name means, so `axiomsOfCut` is how an allowlist entry is checked.
-/

open Lean

namespace Forgive

/-- A heap-owned string representation of a name. The audit runs inside `withImportModules`, whose
region is freed on the way out, so anything that outlives it is copied first. -/
def freshStr (n : Name) : String := (toString n).foldl (fun acc c => acc.push c) ""

/-- The constants `c` refers to directly, in its type, its value, and (for an inductive type) its
constructors. -/
def directRefs (env : Environment) (c : Name) : Array Name :=
  match env.find? c with
  | some (.axiomInfo v) => v.type.getUsedConstants
  | some (.defnInfo v) => v.type.getUsedConstants ++ v.value.getUsedConstants
  | some (.thmInfo v) => v.type.getUsedConstants ++ v.value.getUsedConstants
  | some (.opaqueInfo v) => v.type.getUsedConstants ++ v.value.getUsedConstants
  | some (.quotInfo _) => #[]
  | some (.ctorInfo v) => v.type.getUsedConstants
  | some (.recInfo v) => v.type.getUsedConstants
  | some (.inductInfo v) => v.type.getUsedConstants ++ v.ctors.toArray
  | none => #[]

def isAxiom (env : Environment) (c : Name) : Bool :=
  match env.find? c with
  | some (.axiomInfo _) => true
  | _ => false

/-- A computation over an environment with a memo of each constant's transitive axioms. -/
abbrev AxiomM := ReaderT Environment (StateM (NameMap (Array Name)))

/-- The axioms transitively used by `c`. -/
partial def axiomsOf (c : Name) : AxiomM (Array Name) := do
  if let some s := (← get).find? c then return s
  -- Record `c` empty before recursing, so a cycle's back-edge into it contributes nothing.
  modify (·.insert c #[])
  let env ← read
  let mut used : NameSet := if isAxiom env c then ({} : NameSet).insert c else {}
  for d in directRefs env c do
    for a in (← axiomsOf d) do used := used.insert a
  let arr := used.toArray.qsort Name.lt
  modify (·.insert c arr)
  return arr

/-- The memo of an `axiomsOfCut` traversal. -/
structure CutMemo where
  map : NameMap (Array Name) := {}

/-- The axioms `c` reaches without passing through any name in `cut`. -/
partial def axiomsOfCut (allowed cut : NameSet) (c : Name) :
    StateT CutMemo AxiomM (Array Name) := do
  if cut.contains c then return #[]
  if let some s := (← get).map.find? c then return s
  let full ← axiomsOf c
  if full.all allowed.contains then
    modify fun m => { map := m.map.insert c full }
    return full
  modify fun m => { map := m.map.insert c #[] }
  let env ← read
  let mut used : NameSet := if isAxiom env c then ({} : NameSet).insert c else {}
  for d in directRefs env c do
    for a in (← axiomsOfCut allowed cut d) do used := used.insert a
  let arr := used.toArray.qsort Name.lt
  modify fun m => { map := m.map.insert c arr }
  return arr

end Forgive
