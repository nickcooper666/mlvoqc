open PeanoNat

val merge_path : int list -> int list -> int list

val check_path : int list -> int -> (int -> int -> bool) -> int -> bool

val foralln : (int -> bool) -> int -> bool

val check_graph :
  int -> (int -> int -> int list) -> (int -> int -> bool) -> bool

module LNN :
 sig
  val is_in_graph : int -> int -> int -> bool

  val move_left : int -> int -> int list

  val move_right : int -> int -> int list

  val get_path : int -> int -> int list
 end

module LNNRing :
 sig
  val is_in_graph : int -> int -> int -> bool

  val move_cw : int -> int -> int -> int list

  val move_ccw : int -> int -> int -> int list

  val get_path : int -> int -> int -> int list
 end

module Grid :
 sig
  val is_in_graph : int -> int -> int -> int -> bool

  val row : int -> int -> int

  val col : int -> int -> int

  val move_up : int -> int -> int

  val move_down : int -> int -> int

  val move_left : int -> int

  val move_right : int -> int

  val repeat_move : (int -> int) -> int -> int -> int list

  val get_path : int -> int -> int -> int list
 end

module Tenerife :
 sig
  val tenerife_graph : (int * int) list

  val beq_tup : (int * int) -> (int * int) -> bool

  val is_in_graph : int -> int -> bool

  val get_path : int -> int -> int list
 end
