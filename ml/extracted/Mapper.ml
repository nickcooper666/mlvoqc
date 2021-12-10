open List
open UnitaryListRepresentation

type qmap = (int -> int) * (int -> int)

type matching = (int * int) list

type layer = (int * int) list

(** val fst_tuple : layer -> int list **)

let fst_tuple l =
  map fst l

(** val snd_tuple : layer -> int list **)

let snd_tuple l =
  map snd l

(** val elem_in : int -> int list -> bool **)

let rec elem_in a = function
| [] -> false
| h :: t -> if (=) a h then true else elem_in a t

(** val first_layer : StandardGateSet.standard_ucom_l -> layer -> layer **)

let rec first_layer l la =
  match l with
  | [] -> la
  | g :: t ->
    (match g with
     | App2 (s, n1, n2) ->
       (match s with
        | StandardGateSet.StandardGateSet.U_CX ->
          if (||) (elem_in n1 (fst_tuple la)) (elem_in n2 (snd_tuple la))
          then la
          else first_layer t ((n1, n2) :: la)
        | _ -> first_layer t la)
     | _ -> first_layer t la)

(** val qmapper : matching -> layer -> qmap **)

let rec qmapper mat = function
| [] -> let m1 = fun _ -> 0 in let m2 = fun _ -> 0 in (m1, m2)
| p :: t ->
  let (q1, q2) = p in
  (match t with
   | [] ->
     let (v1, v2) = (fun d l -> match l with [] -> d | h :: _ -> h) (0, 0) mat
     in
     let m1 = fun q -> if (=) q q1 then v1 else v2 in
     let m2 = fun q -> if (=) q v1 then q1 else q2 in (m1, m2)
   | _ :: _ ->
     let (m1, m2) = qmapper (tl mat) t in
     let (v1, v2) = (fun d l -> match l with [] -> d | h :: _ -> h) (0, 0) mat
     in
     let m1' = fun q -> if (=) q q1 then v1 else if (=) q q2 then v2 else m1 q
     in
     let m2' = fun q -> if (=) q v1 then q1 else if (=) q v2 then q2 else m2 q
     in
     (m1', m2'))

(** val initial_qmap : StandardGateSet.standard_ucom_l -> matching -> qmap **)

let initial_qmap l mat =
  qmapper mat (first_layer l [])
