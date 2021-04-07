open UnitaryListRepresentation

type circ = StandardGateSet.standard_ucom_l

type gate_counts =
| BuildCounts of int * int * int * int * int * int * int * int * int * 
   int * int * int * int * int * int * int * int * int * int * int * 
   int

type layout

type c_graph

val get_dim : c_graph -> int

val get_get_path : c_graph -> int -> int -> int list

val get_is_in_graph : c_graph -> int -> int -> bool

val check_well_typed : circ -> int -> bool

val convert_to_ibm : circ -> circ

val convert_to_rzq : circ -> circ

val replace_rzq : circ -> circ

val decompose_to_cnot : circ -> circ

val count_I : circ -> int

val count_X : circ -> int

val count_Y : circ -> int

val count_Z : circ -> int

val count_H : circ -> int

val count_S : circ -> int

val count_T : circ -> int

val count_Sdg : circ -> int

val count_Tdg : circ -> int

val count_Rx : circ -> int

val count_Ry : circ -> int

val count_Rz : circ -> int

val count_Rzq : circ -> int

val count_U1 : circ -> int

val count_U2 : circ -> int

val count_U3 : circ -> int

val count_CX : circ -> int

val count_CZ : circ -> int

val count_SWAP : circ -> int

val count_CCX : circ -> int

val count_CCZ : circ -> int

val count_gates : circ -> gate_counts

val total_gate_count : circ -> int

val count_clifford_rzq : circ -> int

val scale_count : gate_counts -> int -> gate_counts

val add_counts : gate_counts -> gate_counts -> gate_counts

val count_gates_lcr : ((circ * circ) * circ) -> int -> gate_counts

val optimize_ibm : circ -> circ

val optimize_nam : circ -> circ

val optimize_nam_light : circ -> circ

val optimize_nam_lcr : circ -> ((circ * circ) * circ) option

val check_layout : layout -> int -> bool

val check_graph : c_graph -> bool

val check_constraints : circ -> c_graph -> bool

val simple_map : circ -> layout -> c_graph -> circ * layout

val make_tenerife : unit -> c_graph

val make_lnn : int -> c_graph

val make_lnn_ring : int -> c_graph

val make_grid : int -> int -> c_graph

val trivial_layout : int -> layout

val list_to_layout : int list -> layout

val layout_to_list : layout -> int -> int list
