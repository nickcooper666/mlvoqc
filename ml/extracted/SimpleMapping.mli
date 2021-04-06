open Layouts
open UnitaryListRepresentation

val respects_constraints_directed_b :
  (int -> int -> bool) -> StandardGateSet.standard_ucom_l -> bool

val simple_map :
  StandardGateSet.standard_ucom_l -> qmap -> (int -> int -> int list) -> (int
  -> int -> bool) -> StandardGateSet.standard_ucom_l * qmap
