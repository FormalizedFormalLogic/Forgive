import Forgive.Yaml

/-!
# The allowlist

```yaml
version: v0

FFL.some_unproved_lemma:
  forgive:
    - FFL.some_unproved_lemma
FFL.uses_some_unproved_lemma:
  forgive:
    - FFL.some_unproved_lemma
```

Each top-level declaration key lists the axioms or declarations through which its dependencies
may be forgiven. Every entry and forgiven name must be necessary.
-/

open Lean

namespace Forgive

/-- One entry of the allowlist: a declaration and the names it is allowed to depend on. -/
structure ForgiveEntry where
  decl : Name
  forgive : Array Name
  deriving Repr, Inhabited

/-- The parsed allowlist. -/
structure Forgiveness where
  entries : Array ForgiveEntry := #[]
  deriving Repr, Inhabited

namespace Forgiveness

/-- The only format version this reader accepts. -/
def version : String := "v0"

def find? (f : Forgiveness) (decl : Name) : Option ForgiveEntry :=
  f.entries.find? (·.decl == decl)

private def toName (s : String) (what : String) : Except String Name :=
  let n := s.toName
  if n.isAnonymous then throw s!"{what}: `{s}` is not a declaration name" else return n

private def parseEntry (key : String) (v : Yaml.Value) : Except String ForgiveEntry := do
  let decl ← toName key "top-level key"
  let .map fields := v
    | throw s!"`{key}`: expected a mapping with a `forgive` list"
  for (k, _) in fields do
    if k != "forgive" then throw s!"`{key}`: unknown field `{k}` (only `forgive` is allowed)"
  let some (_, forgive) := fields.find? (·.1 == "forgive")
    | throw s!"`{key}`: missing the `forgive` list"
  let .seq items := forgive
    | throw s!"`{key}`: `forgive` must be a list of names"
  if items.isEmpty then throw s!"`{key}`: `forgive` must not be empty"
  let names ← items.mapM fun
    | .scalar s => toName s s!"`{key}`: forgive"
    | _ => throw s!"`{key}`: `forgive` must be a list of names"
  return { decl, forgive := names }

/-- Parse and validate the shape of an allowlist document. -/
def parse (contents : String) : Except String Forgiveness := do
  let doc ← Yaml.parse contents
  let .map entries := doc
    | throw "expected a mapping with a `version` key"
  let some (_, ver) := entries.find? (·.1 == "version")
    | throw "missing `version`"
  let .scalar ver := ver
    | throw "`version` must be a scalar"
  if ver != version then throw s!"unsupported version `{ver}` (expected `{version}`)"
  let mut acc : Array ForgiveEntry := #[]
  for (key, v) in entries do
    if key == "version" then continue
    let e ← parseEntry key v
    if acc.any (·.decl == e.decl) then throw s!"`{key}`: duplicate entry"
    acc := acc.push e
  return { entries := acc }

/-- Read an allowlist from a file. A file that does not exist is an empty allowlist, so a project
that forgives nothing need not carry one. -/
def read (file : System.FilePath) : IO (Except String Forgiveness) := do
  if !(← file.pathExists) then return .ok {}
  return match parse (← IO.FS.readFile file) with
    | .ok fg => .ok fg
    | .error e => .error s!"{file}: {e}"

/-- Every `(entry, name)` whose forgiven `name` the caller has forbidden. `sorryAx` is the
motivating one: an unproved statement is an `axiom` under the name its theorem will keep, so that
the audit reports it by that name instead of collapsing it into `sorryAx`. -/
def forbiddenUses (fg : Forgiveness) (forbidden : List Name) : Array (Name × Name) := Id.run do
  if forbidden.isEmpty then return #[]
  let mut out := #[]
  for e in fg.entries do
    for x in e.forgive do
      if forbidden.contains x then out := out.push (e.decl, x)
  return out

end Forgiveness

end Forgive
