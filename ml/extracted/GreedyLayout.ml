open Datatypes
open FSetAVL
open Layouts
open MappingGateSet
open OrderedTypeEx
open UnitaryListRepresentation

module FSet = Make(Nat_as_OT)

(** val get_default : int list -> FSet.t -> int **)

let rec get_default prefs alloc =
  match prefs with
  | [] -> 0
  | p :: prefs0 -> if FSet.mem p alloc then get_default prefs0 alloc else p

(** val get_unused : int list -> int list -> FSet.t -> int **)

let rec get_unused cands prefs alloc =
  match cands with
  | [] -> get_default prefs alloc
  | c :: cands0 ->
    if FSet.mem c alloc then get_unused cands0 prefs alloc else c

(** val build_layout :
    'a1 coq_Map_Unitary gate_list -> FSet.t -> layout -> (int -> int list) ->
    int list -> FSet.t * layout **)

let rec build_layout l alloc lay get_nearby q_ordering =
  match l with
  | [] -> (alloc, lay)
  | g :: t0 ->
    (match g with
     | App2 (m0, m, n) ->
       (match m0 with
        | UMap_CNOT ->
          (match find_phys lay m with
           | Some m_phys ->
             (match find_phys lay m with
              | Some _ -> build_layout t0 alloc lay get_nearby q_ordering
              | None ->
                let n_phys = get_unused (get_nearby m_phys) q_ordering alloc
                in
                let alloc' = FSet.add n_phys alloc in
                let lay' = add lay n n_phys in
                build_layout t0 alloc' lay' get_nearby q_ordering)
           | None ->
             (match find_phys lay m with
              | Some n_phys ->
                let m_phys = get_unused (get_nearby n_phys) q_ordering alloc
                in
                let alloc' = FSet.add m_phys alloc in
                let lay' = add lay m m_phys in
                build_layout t0 alloc' lay' get_nearby q_ordering
              | None ->
                let m_phys = get_default q_ordering alloc in
                let n_phys = get_unused (get_nearby m_phys) q_ordering alloc
                in
                let alloc' = FSet.add n_phys (FSet.add m_phys alloc) in
                let lay' = add (add lay m m_phys) n n_phys in
                build_layout t0 alloc' lay' get_nearby q_ordering))
        | _ -> build_layout t0 alloc lay get_nearby q_ordering)
     | _ -> build_layout t0 alloc lay get_nearby q_ordering)

(** val extend_layout : int -> FSet.t -> layout -> int list -> layout **)

let rec extend_layout n alloc lay q_ordering =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> lay)
    (fun n' ->
    match find_phys lay n' with
    | Some _ -> extend_layout n' alloc lay q_ordering
    | None ->
      let m = get_default q_ordering alloc in
      extend_layout n' (FSet.add m alloc) (add lay n' m) q_ordering)
    n

(** val greedy_layout :
    'a1 coq_Map_Unitary gate_list -> int -> (int -> int list) -> int list ->
    layout **)

let greedy_layout l n get_nearby q_ordering =
  let (alloc, lay) = build_layout l FSet.empty empty get_nearby q_ordering in
  extend_layout n alloc lay q_ordering
