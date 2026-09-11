import Lean

/-!
# What to audit

`Config` is the whole of the audit's project-specific input: every name, path, and heading the
audit and its reports mention comes from here, so the same executable serves any library.
-/

open Lean

namespace Forgive

/-- What to audit, and where to write the reports. -/
structure Config where
  /-- The root modules to audit. A declaration is a candidate when the module defining it is a
  root or one of its submodules. -/
  roots : Array Name := #[]
  /-- The modules to import before the audit. Empty means the roots themselves. -/
  imports : Array Name := #[]
  /-- The axioms every declaration may use. -/
  allowed : List Name := [``propext, ``Classical.choice, ``Quot.sound]
  /-- Names no allowlist entry may forgive. -/
  forbidden : List Name := []
  /-- The allowlist, relative to the working directory. -/
  forgiveFile : System.FilePath := "forgive.yml"
  /-- Where the machine-readable report goes, or `none` to write none. -/
  jsonFile? : Option System.FilePath := some (".lake" / "audit.json")
  /-- Where the Markdown report goes, or `none` to write none. -/
  markdownFile? : Option System.FilePath := some (".lake" / "audit.md")
  /-- The heading of the Markdown report. -/
  title : String := "Axiom audit"

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

end Forgive
