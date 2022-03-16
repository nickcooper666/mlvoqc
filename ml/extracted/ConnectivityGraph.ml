open PeanoNat

module LNN =
 struct
  (** val is_in_graph : int -> int -> int -> bool **)

  let is_in_graph dim n1 n2 =
    (||)
      ((&&) (Nat.ltb ((+) n1 (Pervasives.succ 0)) dim)
        ((=) n2 ((+) n1 (Pervasives.succ 0))))
      ((&&) (Nat.ltb ((+) n2 (Pervasives.succ 0)) dim)
        ((=) n1 ((+) n2 (Pervasives.succ 0))))

  (** val move_left : int -> int -> int list **)

  let rec move_left n dist =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> n :: [])
      (fun n' -> n :: (move_left ((-) n (Pervasives.succ 0)) n'))
      dist

  (** val move_right : int -> int -> int list **)

  let rec move_right n dist =
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> n :: [])
      (fun n' -> n :: (move_right ((+) n (Pervasives.succ 0)) n'))
      dist

  (** val get_path : int -> int -> int list **)

  let get_path n1 n2 =
    if Nat.ltb n1 n2
    then move_right n1 ((-) n2 n1)
    else if Nat.ltb n2 n1 then move_left n1 ((-) n1 n2) else n1 :: []

  (** val interleave : int list -> int list -> int list **)

  let rec interleave l1 l2 =
    match l1 with
    | [] -> l2
    | h1 :: t1 ->
      (match l2 with
       | [] -> l1
       | h2 :: t2 -> h1 :: (h2 :: (interleave t1 t2)))

  (** val qubit_ordering : int -> int list **)

  let qubit_ordering dim =
    if (=) dim 0
    then []
    else if (=) dim (Pervasives.succ 0)
         then 0 :: []
         else interleave
                (get_path (Nat.div dim (Pervasives.succ (Pervasives.succ 0)))
                  ((-) dim (Pervasives.succ 0)))
                (get_path
                  ((-) (Nat.div dim (Pervasives.succ (Pervasives.succ 0)))
                    (Pervasives.succ 0)) 0)

  (** val get_nearby : int -> int -> int list **)

  let get_nearby dim n =
    if Nat.ltb dim (Pervasives.succ (Pervasives.succ 0))
    then []
    else if (=) n 0
         then get_path (Pervasives.succ 0) ((-) dim (Pervasives.succ 0))
         else if (=) n ((-) dim (Pervasives.succ 0))
              then get_path ((-) dim (Pervasives.succ (Pervasives.succ 0))) 0
              else interleave (get_path ((-) n (Pervasives.succ 0)) 0)
                     (get_path ((+) n (Pervasives.succ 0))
                       ((-) dim (Pervasives.succ 0)))
 end
