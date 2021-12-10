open Printf
open Voqc.Qasm
open Voqc.Main
open Voqc.Mapper

(** Minimal example of running initial_qmap: **)

(* print qmap as a list *)
let rec print_qmap qm n =
  if n = 0 then () 
  else (printf "%d -> %d\n" n (fst qm n) ; print_qmap qm (n-1));;

(* read an input QASM file (this program uses 5 qubits so n will be 5) *)
let (c, n) = read_qasm "benchmarks/VOQC-benchmarks/Arithmetic_and_Toffoli/tof_3.qasm";;

(* get rid of multi-qubit gates that aren't CNOTs *)
let c = convert_to_rzq c;;

(* some fake matching on a 6-qubit graph *)
let mat = [(0,1); (2,3); (4,5)];;

(* run initial_qmap *)
let qm = initial_qmap c mat;;

(* print layout (using 6 qubits) *)
print_qmap qm 6
