import Lean

/-!
# A YAML subset reader

A reader for the YAML subset an allowlist is written in: block mappings, block sequences, flow
sequences of scalars, one layer of quoting, and `#` comments. Anchors, multi-line scalars,
flow mappings, and documents separated by `---` are not supported.
-/

namespace Forgive.Yaml

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

end Forgive.Yaml
