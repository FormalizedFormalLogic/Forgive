/-! Fixtures: a proof with no axioms, one with allowed axioms, and an unproved statement. -/

namespace TestLib

theorem clean : 1 + 1 = 2 := rfl

theorem allowed (p : Prop) : p ∨ ¬p := Classical.em p

/-- Unproved, declared under the name its theorem will keep. -/
axiom add_comm' (m n : Nat) : m + n = n + m

theorem uses_add_comm' : 1 + 2 = 2 + 1 := add_comm' 1 2

end TestLib
