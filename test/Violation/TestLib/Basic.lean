/-! A fixture whose axiom the allowlist does not permit. -/

namespace TestLib

/-- Unproved, and forgiven nowhere. -/
axiom add_comm' (m n : Nat) : m + n = n + m

theorem uses_add_comm' : 1 + 2 = 2 + 1 := add_comm' 1 2

end TestLib
