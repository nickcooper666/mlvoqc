open PeanoNat

type qmap = (int -> int) * (int -> int)

val log2phys : qmap -> int -> int

val phys2log : qmap -> int -> int

val swap_in_map : qmap -> int -> int -> qmap

val layout_well_formed_b : int -> qmap -> bool

val layout_to_list : int -> qmap -> int list

val list_to_layout : int list -> qmap

val trivial_layout : int -> qmap
