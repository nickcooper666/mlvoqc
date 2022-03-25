open CXCancellation
open ConnectivityGraph
open GateCancellation
open GreedyLayout
open HadamardReduction
open Layouts
open List
open MappingGateSet
open MappingValidation
open NotPropagation
open Optimize1qGates
open RotationMerging
open SwapRoute
open UnitaryListRepresentation

type circ = FullGateSet.full_ucom_l

(** val check_well_typed : circ -> int -> bool **)

let check_well_typed c n =
  uc_well_typed_l_b n ((fun x _ -> x) c n)

(** val convert_to_ibm : circ -> FullGateSet.full_ucom_l **)

let convert_to_ibm =
  FullGateSet.convert_to_ibm

(** val convert_to_rzq : circ -> FullGateSet.full_ucom_l **)

let convert_to_rzq =
  FullGateSet.convert_to_rzq

(** val replace_rzq :
    circ -> FullGateSet.FullGateSet.coq_Full_Unitary gate_list **)

let replace_rzq =
  FullGateSet.replace_rzq

(** val decompose_to_cnot :
    circ -> FullGateSet.FullGateSet.coq_Full_Unitary gate_list **)

let decompose_to_cnot =
  FullGateSet.decompose_to_cnot

(** val count_I : circ -> int **)

let count_I l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_I -> true
         | _ -> false)
      | _ -> false) l)

(** val count_X : circ -> int **)

let count_X l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_X -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Y : circ -> int **)

let count_Y l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_Y -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Z : circ -> int **)

let count_Z l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_Z -> true
         | _ -> false)
      | _ -> false) l)

(** val count_H : circ -> int **)

let count_H l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_H -> true
         | _ -> false)
      | _ -> false) l)

(** val count_S : circ -> int **)

let count_S l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_S -> true
         | _ -> false)
      | _ -> false) l)

(** val count_T : circ -> int **)

let count_T l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_T -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Sdg : circ -> int **)

let count_Sdg l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_Sdg -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Tdg : circ -> int **)

let count_Tdg l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_Tdg -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Rx : circ -> int **)

let count_Rx l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_Rx _ -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Ry : circ -> int **)

let count_Ry l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_Ry _ -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Rz : circ -> int **)

let count_Rz l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_Rz _ -> true
         | _ -> false)
      | _ -> false) l)

(** val count_Rzq : circ -> int **)

let count_Rzq l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_Rzq _ -> true
         | _ -> false)
      | _ -> false) l)

(** val count_U1 : circ -> int **)

let count_U1 l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_U1 _ -> true
         | _ -> false)
      | _ -> false) l)

(** val count_U2 : circ -> int **)

let count_U2 l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_U2 (_, _) -> true
         | _ -> false)
      | _ -> false) l)

(** val count_U3 : circ -> int **)

let count_U3 l =
  List.length
    (List.filter (fun g ->
      match g with
      | App1 (y, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_U3 (_, _, _) -> true
         | _ -> false)
      | _ -> false) l)

(** val count_CX : circ -> int **)

let count_CX l =
  List.length
    (List.filter (fun g ->
      match g with
      | App2 (y, _, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_CX -> true
         | _ -> false)
      | _ -> false) l)

(** val count_CZ : circ -> int **)

let count_CZ l =
  List.length
    (List.filter (fun g ->
      match g with
      | App2 (y, _, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_CZ -> true
         | _ -> false)
      | _ -> false) l)

(** val count_SWAP : circ -> int **)

let count_SWAP l =
  List.length
    (List.filter (fun g ->
      match g with
      | App2 (y, _, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_SWAP -> true
         | _ -> false)
      | _ -> false) l)

(** val count_CCX : circ -> int **)

let count_CCX l =
  List.length
    (List.filter (fun g ->
      match g with
      | App3 (y, _, _, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_CCX -> true
         | _ -> false)
      | _ -> false) l)

(** val count_CCZ : circ -> int **)

let count_CCZ l =
  List.length
    (List.filter (fun g ->
      match g with
      | App3 (y, _, _, _) ->
        (match y with
         | FullGateSet.FullGateSet.U_CCZ -> true
         | _ -> false)
      | _ -> false) l)

(** val count_1q : circ -> int **)

let count_1q l =
  List.length
    (List.filter (fun g -> match g with
                           | App1 (_, _) -> true
                           | _ -> false) l)

(** val count_2q : circ -> int **)

let count_2q l =
  List.length
    (List.filter (fun g -> match g with
                           | App2 (_, _, _) -> true
                           | _ -> false) l)

(** val count_3q : circ -> int **)

let count_3q l =
  List.length
    (List.filter (fun g ->
      match g with
      | App3 (_, _, _, _) -> true
      | _ -> false) l)

(** val count_total : circ -> int **)

let count_total =
  List.length

(** val count_rzq_clifford : circ -> int **)

let count_rzq_clifford l =
  let f = fun g ->
    match g with
    | App1 (y, _) ->
      (match y with
       | FullGateSet.FullGateSet.U_Rzq q ->
         let q' = RzQGateSet.bound q in
         (||)
           ((||)
             ((||) (Q.equal q' (Q.of_int 0)) (Q.equal q' (Q.of_ints 1 2)))
             (Q.equal q' (Q.of_ints 3 2))) (Q.equal q' (Q.of_int 1))
       | _ -> false)
    | _ -> false
  in
  List.length (List.filter f l)

(** val optimize_1q_gates : circ -> circ **)

let optimize_1q_gates c =
  FullGateSet.coq_IBM_to_full (optimize_1q_gates (FullGateSet.full_to_IBM c))

(** val cx_cancellation : circ -> circ **)

let cx_cancellation c =
  FullGateSet.coq_IBM_to_full (cx_cancellation (FullGateSet.full_to_IBM c))

(** val optimize_ibm : circ -> circ **)

let optimize_ibm c =
  FullGateSet.coq_IBM_to_full
    (CXCancellation.cx_cancellation
      (Optimize1qGates.optimize_1q_gates (FullGateSet.full_to_IBM c)))

(** val not_propagation : circ -> circ **)

let not_propagation c =
  FullGateSet.coq_RzQ_to_full (not_propagation (FullGateSet.full_to_RzQ c))

(** val hadamard_reduction : circ -> circ **)

let hadamard_reduction c =
  FullGateSet.coq_RzQ_to_full (hadamard_reduction (FullGateSet.full_to_RzQ c))

(** val cancel_single_qubit_gates : circ -> circ **)

let cancel_single_qubit_gates c =
  FullGateSet.coq_RzQ_to_full
    (cancel_single_qubit_gates (FullGateSet.full_to_RzQ c))

(** val cancel_two_qubit_gates : circ -> circ **)

let cancel_two_qubit_gates c =
  FullGateSet.coq_RzQ_to_full
    (cancel_two_qubit_gates (FullGateSet.full_to_RzQ c))

(** val merge_rotations : circ -> circ **)

let merge_rotations c =
  FullGateSet.coq_RzQ_to_full (merge_rotations (FullGateSet.full_to_RzQ c))

(** val optimize_nam : circ -> circ **)

let optimize_nam c =
  FullGateSet.coq_RzQ_to_full
    (GateCancellation.cancel_single_qubit_gates
      (GateCancellation.cancel_two_qubit_gates
        (RotationMerging.merge_rotations
          (GateCancellation.cancel_single_qubit_gates
            (HadamardReduction.hadamard_reduction
              (GateCancellation.cancel_two_qubit_gates
                (GateCancellation.cancel_single_qubit_gates
                  (GateCancellation.cancel_two_qubit_gates
                    (HadamardReduction.hadamard_reduction
                      (NotPropagation.not_propagation
                        (FullGateSet.full_to_RzQ c)))))))))))

(** val optimize_nam_light : circ -> circ **)

let optimize_nam_light c =
  FullGateSet.coq_RzQ_to_full
    (GateCancellation.cancel_single_qubit_gates
      (HadamardReduction.hadamard_reduction
        (GateCancellation.cancel_two_qubit_gates
          (GateCancellation.cancel_single_qubit_gates
            (GateCancellation.cancel_two_qubit_gates
              (HadamardReduction.hadamard_reduction
                (NotPropagation.not_propagation (FullGateSet.full_to_RzQ c))))))))

(** val optimize_nam_lcr : circ -> ((circ * circ) * circ) option **)

let optimize_nam_lcr c =
  coq_LCR c optimize_nam FullGateSet.FullGateSet.match_gate

type layout = Layouts.layout

type c_graph = int * (int -> int -> bool)

(** val graph_dim : c_graph -> int **)

let graph_dim =
  fst

(** val is_in_graph : c_graph -> int -> int -> bool **)

let is_in_graph =
  snd

type ext_c_graph =
  ((c_graph * (int -> int -> int list)) * int list) * (int -> int list)

(** val get_c_graph : ext_c_graph -> c_graph **)

let get_c_graph ecg =
  fst (fst (fst ecg))

(** val get_path : ext_c_graph -> int -> int -> int list **)

let get_path ecg =
  snd (fst (fst ecg))

(** val qubit_ordering : ext_c_graph -> int list **)

let qubit_ordering ecg =
  snd (fst ecg)

(** val get_nearby_qubits : ext_c_graph -> int -> int list **)

let get_nearby_qubits =
  snd

(** val swap_route :
    circ -> layout -> ext_c_graph -> FullGateSet.full_ucom_l **)

let swap_route c lay ecg =
  let n = graph_dim (get_c_graph ecg) in
  let (c0, _) =
    swap_route (FullGateSet.full_to_map ((fun x _ -> x) c n)) lay
      (get_path ecg)
  in
  FullGateSet.map_to_full c0

(** val decompose_swaps : circ -> c_graph -> FullGateSet.full_ucom_l **)

let decompose_swaps c cg =
  FullGateSet.map_to_full
    (decompose_swaps_and_cnots (FullGateSet.full_to_map c) (is_in_graph cg))

(** val trivial_layout : int -> layout **)

let trivial_layout =
  trivial_layout

(** val check_list : int list -> bool **)

let check_list =
  check_list

(** val list_to_layout : Layouts.FMap.key list -> layout **)

let list_to_layout =
  list_to_layout

(** val layout_to_list : layout -> int -> int list **)

let layout_to_list lay n =
  map (fun ox -> match ox with
                 | Some x -> x
                 | None -> 0) (layout_to_list n lay)

(** val greedy_layout : circ -> ext_c_graph -> layout **)

let greedy_layout c ecg =
  greedy_layout (FullGateSet.full_to_map c) (graph_dim (get_c_graph ecg))
    (get_nearby_qubits ecg) (qubit_ordering ecg)

(** val beq_tup : (int * int) -> (int * int) -> bool **)

let beq_tup t t' =
  let (n1, n2) = t in let (n1', n2') = t' in (&&) ((=) n1 n1') ((=) n2 n2')

(** val c_graph_from_coupling_map : int -> (int * int) list -> c_graph **)

let c_graph_from_coupling_map n cmap =
  (n, (fun n1 n2 -> List.exists (beq_tup (n1, n2)) cmap))

(** val lnn_c_graph : int -> c_graph **)

let lnn_c_graph n =
  (n, (LNN.is_in_graph n))

(** val lnn_ext_c_graph : int -> ext_c_graph **)

let lnn_ext_c_graph n =
  ((((lnn_c_graph n), LNN.get_path), (LNN.qubit_ordering n)),
    (LNN.get_nearby n))

(** val remove_swaps : circ -> layout -> FullGateSet.full_ucom_l **)

let remove_swaps c lay =
  let (c0, _) = remove_swaps (FullGateSet.full_to_map c) lay in
  FullGateSet.map_to_full c0

(** val check_swap_equivalence : circ -> circ -> layout -> layout -> bool **)

let check_swap_equivalence c1 c2 lay1 lay2 =
  is_swap_equivalent (FullGateSet.full_to_map c1)
    (FullGateSet.full_to_map c2) lay1 lay2 (fun n ->
    match_gate n (FullGateSet.FullGateSet.match_gate (Pervasives.succ 0)))

(** val check_constraints : circ -> c_graph -> bool **)

let check_constraints c cg =
  check_constraints (FullGateSet.full_to_map c) (is_in_graph cg)
