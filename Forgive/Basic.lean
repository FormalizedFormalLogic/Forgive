import Lean

/-!
# The audit

An axiom audit for Lean 4 libraries: every declaration under an audited root must reach only the
axioms the allowlist accepts, except where an entry forgives a name it reaches through.
-/

open Lean

namespace Forgive

/-! ## A YAML subset reader

A reader for the YAML subset an allowlist is written in: block mappings, block sequences, flow
sequences of scalars, one layer of quoting, and `#` comments. Anchors, multi-line scalars,
flow mappings, and documents separated by `---` are not supported.
-/

namespace Yaml

/-- The parsed document. -/
inductive Value where
  | null
  | scalar (s : String)
  | seq (items : Array Value)
  | map (entries : Array (String × Value))
  deriving Repr, Inhabited

/-- A source line represented by its 1-based number, indentation, and trimmed uncommented text. -/
structure Line where
  no : Nat
  indent : Nat
  text : String
  deriving Repr

private def ofChars (cs : List Char) : String := cs.foldl (·.push ·) ""

private def trimChars (cs : List Char) : List Char :=
  (cs.dropWhile Char.isWhitespace).reverse.dropWhile Char.isWhitespace |>.reverse

private def trim (s : String) : String := ofChars (trimChars s.toList)

/-- Drop a trailing comment: `#` at the start of the text or after whitespace. -/
private def stripComment (cs : List Char) : List Char :=
  go ' ' cs
where
  go (prev : Char) : List Char → List Char
    | [] => []
    | '#' :: rest => if prev.isWhitespace then [] else '#' :: go '#' rest
    | c :: rest => c :: go c rest

/-- Split a document into meaningful lines. Fails on a tab in the indentation. -/
private def toLines (s : String) : Except String (Array Line) := do
  let mut out := #[]
  let mut no := 0
  for raw in s.splitOn "\n" do
    no := no + 1
    let cs := stripComment raw.toList
    let leading := cs.takeWhile fun c => c == ' ' || c == '\t'
    if leading.contains '\t' then
      throw s!"line {no}: tabs are not allowed in indentation"
    let text := trimChars cs
    if !text.isEmpty then
      out := out.push { no, indent := leading.length, text := ofChars text }
  return out

/-- Remove one layer of matching `"` or `'` quotes. No escape sequences are interpreted. -/
private def unquote (s : String) : String :=
  match s.toList with
  | '"' :: rest => if rest.getLast? == some '"' then ofChars rest.dropLast else s
  | '\'' :: rest => if rest.getLast? == some '\'' then ofChars rest.dropLast else s
  | _ => s

/-- A scalar, or a flow sequence `[a, b]` of scalars. -/
private def parseInline (s : String) (no : Nat) : Except String Value :=
  match s.toList with
  | '[' :: rest =>
    if rest.getLast? != some ']' then
      throw s!"line {no}: unterminated flow sequence"
    else
      let inner := ofChars rest.dropLast
      let items := (inner.splitOn ",").map trim |>.filter (!·.isEmpty)
      return .seq (items.map (.scalar ∘ unquote)).toArray
  | _ => return .scalar (unquote s)

private def isSeqItem (t : String) : Bool := t == "-" || t.startsWith "- "

/-- Split `key: value` (or `key:`) at the first `:` followed by a space or the end of the line. -/
private def splitKey (t : String) (no : Nat) : Except String (String × String) :=
  match go [] t.toList with
  | some (k, v) =>
    let k := unquote (ofChars (trimChars k))
    if k.isEmpty then throw s!"line {no}: empty key" else return (k, ofChars (trimChars v))
  | none => throw s!"line {no}: expected `key: value` or `- item`, got `{t}`"
where
  go (acc : List Char) : List Char → Option (List Char × List Char)
    | ':' :: [] => some (acc.reverse, [])
    | ':' :: ' ' :: rest => some (acc.reverse, rest)
    | c :: rest => go (c :: acc) rest
    | [] => none

mutual

/-- Parse the block starting at line `i`; returns the value and the index of the first line after
it. -/
private partial def parseBlock (ls : Array Line) (i : Nat) : Except String (Value × Nat) := do
  let some l := ls[i]? | return (.null, i)
  if isSeqItem l.text then parseSeq ls i l.indent #[] else parseMap ls i l.indent #[]

private partial def parseSeq (ls : Array Line) (i indent : Nat) (acc : Array Value) :
    Except String (Value × Nat) := do
  let some l := ls[i]? | return (.seq acc, i)
  if l.indent < indent then return (.seq acc, i)
  if l.indent > indent then throw s!"line {l.no}: unexpected indentation"
  if !isSeqItem l.text then throw s!"line {l.no}: expected a `- item` in this sequence"
  let body := ofChars (trimChars (l.text.toList.drop 1))
  if body.isEmpty then throw s!"line {l.no}: a sequence item must be a scalar on the same line"
  parseSeq ls (i + 1) indent (acc.push (← parseInline body l.no))

private partial def parseMap (ls : Array Line) (i indent : Nat) (acc : Array (String × Value)) :
    Except String (Value × Nat) := do
  let some l := ls[i]? | return (.map acc, i)
  if l.indent < indent then return (.map acc, i)
  if l.indent > indent then throw s!"line {l.no}: unexpected indentation"
  if isSeqItem l.text then throw s!"line {l.no}: expected a `key:` in this mapping"
  let (key, rest) ← splitKey l.text l.no
  if acc.any (·.1 == key) then throw s!"line {l.no}: duplicate key `{key}`"
  if !rest.isEmpty then
    return ← parseMap ls (i + 1) indent (acc.push (key, ← parseInline rest l.no))
  match ls[i + 1]? with
  | some l' =>
    if l'.indent > indent then
      let (v, j) ← parseBlock ls (i + 1)
      parseMap ls j indent (acc.push (key, v))
    else
      parseMap ls (i + 1) indent (acc.push (key, .null))
  | none => return (.map (acc.push (key, .null)), i + 1)

end

/-- Parse a document. An empty document is `.null`. -/
def parse (s : String) : Except String Value := do
  let ls ← toLines s
  let (v, j) ← parseBlock ls 0
  if let some l := ls[j]? then
    throw s!"line {l.no}: unexpected indentation"
  return v

end Yaml

/-! ## What to audit

`Config` is the whole of the audit's project-specific input, so the same executable serves any
library.
-/

/-- What to audit, and where to write the report. -/
structure Config where
  /-- The root modules to audit. A declaration is a candidate when the module defining it is a
  root or one of its submodules. -/
  roots : Array Name := #[]
  /-- The modules to import before the audit. Empty means the roots themselves. -/
  imports : Array Name := #[]
  /-- The axioms every declaration may use, unless the allowlist's `accept` replaces them. -/
  allowed : List Name := [``propext, ``Classical.choice, ``Quot.sound]
  /-- The allowlist, relative to the working directory. -/
  forgiveFile : System.FilePath := "forgive.yml"
  /-- Where the JSON report goes, or `none` to write none. -/
  jsonFile? : Option System.FilePath := none

namespace Config

/-- The modules to load: the explicit imports, or the roots when there are none. -/
def modules (cfg : Config) : Array Name :=
  if cfg.imports.isEmpty then cfg.roots else cfg.imports

/-- Is `mod` an audited root or one of its submodules? -/
def covers (cfg : Config) (mod : Name) : Bool :=
  cfg.roots.any fun r => mod == r || r.isPrefixOf mod

/-- The roots as a comma-separated list of quoted names, for a message. -/
def rootsStr (cfg : Config) : String :=
  ", ".intercalate (cfg.roots.map (s!"`{·}`")).toList

end Config

/-! ## The allowlist

```yaml
version: v0

accept:
  - propext
  - Quot.sound
  - Classical.choice

declaration:
  MyLib.some_unproved_lemma:
    forgive:
      - MyLib.some_unproved_lemma
  MyLib.uses_some_unproved_lemma:
    forgive:
      - MyLib.some_unproved_lemma
```

`accept` is the axioms every declaration may use. Each key under `declaration` lists the axioms or
declarations through which its dependencies may be forgiven. Every entry and forgiven name must be
necessary.
-/

/-- One entry of the allowlist: a declaration and the names it is allowed to depend on. -/
structure ForgiveEntry where
  declaration : Name
  forgive : Array Name
  deriving Repr, Inhabited

/-- The parsed allowlist. -/
structure Forgiveness where
  /-- The `accept` list, which replaces the configured axioms. `none` when the file omits it. -/
  accept? : Option (List Name) := none
  entries : Array ForgiveEntry := #[]
  deriving Repr, Inhabited

namespace Forgiveness

/-- The only format version this reader accepts. -/
def version : String := "v0"

def find? (f : Forgiveness) (declaration : Name) : Option ForgiveEntry :=
  f.entries.find? (·.declaration == declaration)

private def toName (s : String) (what : String) : Except String Name :=
  let n := s.toName
  if n.isAnonymous then throw s!"{what}: `{s}` is not a declaration name" else return n

private def parseNames (what : String) : Yaml.Value → Except String (Array Name)
  | .seq items => items.mapM fun
    | .scalar s => toName s what
    | _ => throw s!"{what}: expected a list of names"
  | _ => throw s!"{what}: expected a list of names"

private def parseEntry (key : String) (v : Yaml.Value) : Except String ForgiveEntry := do
  let declaration ← toName key "`declaration` key"
  let .map fields := v
    | throw s!"`{key}`: expected a mapping with a `forgive` list"
  for (k, _) in fields do
    if k != "forgive" then throw s!"`{key}`: unknown field `{k}` (only `forgive` is allowed)"
  let some (_, forgive) := fields.find? (·.1 == "forgive")
    | throw s!"`{key}`: missing the `forgive` list"
  let names ← parseNames s!"`{key}`: `forgive`" forgive
  if names.isEmpty then throw s!"`{key}`: `forgive` must not be empty"
  return { declaration, forgive := names }

private def parseEntries : Yaml.Value → Except String (Array ForgiveEntry)
  | .null => return #[]
  | .map ds => do
    let mut acc : Array ForgiveEntry := #[]
    for (key, v) in ds do
      let e ← parseEntry key v
      if acc.any (·.declaration == e.declaration) then throw s!"`{key}`: duplicate entry"
      acc := acc.push e
    return acc
  | _ => throw "`declaration`: expected a mapping of declaration names"

/-- Parse and validate the shape of an allowlist document. -/
def parse (contents : String) : Except String Forgiveness := do
  let .map fields ← Yaml.parse contents
    | throw "expected a mapping with a `version` key"
  let some (_, ver) := fields.find? (·.1 == "version")
    | throw "missing `version`"
  let .scalar ver := ver
    | throw "`version` must be a scalar"
  if ver != version then throw s!"unsupported version `{ver}` (expected `{version}`)"
  let mut fg : Forgiveness := {}
  for (key, v) in fields do
    match key with
    | "version" => pure ()
    | "accept" => fg := { fg with accept? := some (← parseNames "`accept`" v).toList }
    | "declaration" => fg := { fg with entries := ← parseEntries v }
    | k => throw s!"unknown key `{k}` (expected `version`, `accept`, or `declaration`)"
  return fg

/-- Read an allowlist from a file. A file that does not exist is an empty allowlist, so a project
that forgives nothing need not carry one. -/
def read (file : System.FilePath) : IO (Except String Forgiveness) := do
  if !(← file.pathExists) then return .ok {}
  return match parse (← IO.FS.readFile file) with
    | .ok fg => .ok fg
    | .error e => .error s!"{file}: {e}"

end Forgiveness

/-! ## Collecting axioms

The transitive axioms of a constant, and the same traversal with a set of names cut out of it:
cutting is what forgiving a name means, so `axiomsOfCut` is how an allowlist entry is checked.
-/

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

/-! ## The audit

Every declaration under an audited root is checked against the allowed axioms; an allowlist entry
is honoured only when cutting the names it forgives leaves nothing disallowed behind, and every
entry and every forgiven name must earn its place.
-/

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
  /-- Problems with the allowlist: an entry whose declaration is unknown or clean, and a
  forgiven name that is unknown or redundant. -/
  forgiveErrors : Array String

/-- Whether the audit has no violations or allowlist errors. -/
def Report.ok (r : Report) : Bool := r.violations.isEmpty && r.forgiveErrors.isEmpty

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
  let mut r : Analysis := {}
  let err (r : Analysis) (msg : String) : Analysis := { r with errors := r.errors.push msg }
  let candSet : NameSet := candidates.foldl (·.insert ·) {}
  for e in fg.entries do
    if !env.contains e.declaration then
      r := err r s!"`{freshStr e.declaration}` is not a declaration"
    else if !candSet.contains e.declaration then
      r := err r s!"`{freshStr e.declaration}` is not defined under {cfg.rootsStr}"
    for x in e.forgive do
      if !env.contains x then
        r := err r s!"`{freshStr e.declaration}`: `{freshStr x}` is neither a declaration nor an axiom"
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

/-- The configuration the allowlist's `accept` list asks for. -/
def Config.accepting (cfg : Config) (fg : Forgiveness) : Config :=
  match fg.accept? with
  | some ns => { cfg with allowed := ns }
  | none => cfg

/-- Audit every declaration defined under `cfg.roots`, against `cfg.allowed` and the allowlist. -/
def audit (cfg : Config) (fg : Forgiveness) : CoreM Report := do
  let cfg := cfg.accepting fg
  let env ← getEnv
  let allowedSet : NameSet := cfg.allowed.foldl (·.insert ·) {}
  let modNames := env.allImportedModuleNames
  -- Candidates: declarations defined in a module under a root.
  let candidates : Array Name := env.constants.fold (init := #[]) fun acc name _ =>
    match env.getModuleIdxFor? name with
    | some idx =>
      match modNames[idx.toNat]? with
      | some m => if cfg.covers m then acc.push name else acc
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

/-! ## The JSON report -/

private def pair (p : String × Array String) : Json :=
  Json.mkObj [("declaration", Json.str p.1), ("axioms", Lean.toJson p.2)]

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
