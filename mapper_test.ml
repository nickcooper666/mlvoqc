(* dune exec ./mapper_test.exe *)

open Printf
open Voqc.Qasm
open Voqc.Main

let _ = Sys.chdir "mapping_validation_tests";;

let read_layout f =
  let ic = open_in f in
  try 
    let line = input_line ic in
    close_in ic;
    List.map int_of_string (Str.split (Str.regexp " +") line)
  with e ->
    close_in_noerr ic;
    raise e;;

let read_circ f n =
  let (c, _) = read_qasm f in
  (c, trivial_layout n);;

let read_circ_and_layout f1 f2 =
  let (c, _) = read_qasm f1 in
  let l = read_layout f2 in
  (c, list_to_layout l);;

printf "Checking handwritten tests...\n";;

let (ca, la) = read_circ "test1a.qasm" 3 in
let (cb, lb) = read_circ "test1b.qasm" 3 in
let test1 = check_swap_equivalence ca cb la lb in

let (ca, la) = read_circ "test2a.qasm" 3 in
let (cb, lb) = read_circ "test2b.qasm" 3 in
let test2 = check_swap_equivalence ca cb la lb in

let (ca, la) = read_circ "test3a.qasm" 3 in
let (cb, lb) = read_circ "test3b.qasm" 3 in
let test3 = check_swap_equivalence ca cb la lb in

let (ca, la) = read_circ "test4a.qasm" 3 in
let (cb, lb) = read_circ_and_layout "test4b.qasm" "test4b.layout" in
let test4 = check_swap_equivalence ca cb la lb in

if test1 && test2 && test3 && test4
then printf "\tPASSED\n"
else printf "\tFAILED\n";;

printf "Checking Qiskit-generated tests...\n";;

(* test equivalence between every pair of files *)
let run_test l0 =
  let read name_and_size =
    let (fname, n) = name_and_size in 
    if Sys.file_exists (fname ^ ".layout") 
    then read_circ_and_layout (fname ^ ".qasm") (fname ^ ".layout") 
    else read_circ (fname ^ ".qasm") n in
  let l0 = List.map read l0 in
  let rec inner_loop c1 l1 l = 
    match l with [] -> true
    | (c2,l2) :: t -> 
      if check_swap_equivalence c1 c2 l1 l2 then inner_loop c1 l1 t else false in
  let rec outer_loop l = 
    match l with [] -> true 
    | (c1,l1) :: t -> if inner_loop c1 l1 l0 then outer_loop t else false in
  outer_loop l0;;

printf "\tqiskit1 - ";
let res = run_test [("qiskit1a", 7); ("qiskit1b", 7); ("qiskit1c", 7); ("qiskit1d", 7)] in
if res then printf "PASSED\n" else printf "FAILED\n";

printf "\tqiskit2 - ";
let res = run_test [("qiskit2a", 15); ("qiskit2b", 15); ("qiskit2c", 15); ("qiskit2d", 15); ("qiskit2e", 15)] in
if res then printf "PASSED\n" else printf "FAILED\n";

printf "\tqiskit3 - ";
let res = run_test [("qiskit3a", 15); ("qiskit3b", 15); ("qiskit3c", 15); ("qiskit3d", 15); ("qiskit3e", 15)] in
if res then printf "PASSED\n" else printf "FAILED\n";

printf "\tqiskit4 - ";
let res = run_test [("qiskit4a", 15); ("qiskit4b", 15); ("qiskit4c", 15); ("qiskit4e", 15)] in
if res then printf "PASSED\n" else printf "FAILED\n";

printf "Checking handwritten error-case tests...\n";;

let (ca, la) = read_circ "qiskit2a.qasm" 15 in
let (cb, lb) = read_circ_and_layout "qiskit2c.qasm" "qiskit2c.layout.error" in
let test1 = check_swap_equivalence ca cb la lb in

let (ca, la) = read_circ "qiskit3b.qasm" 15 in
let (cb, lb) = read_circ_and_layout "qiskit3c.qasm.error" "qiskit3c.layout" in
let test2 = check_swap_equivalence ca cb la lb in

let (ca, la) = read_circ_and_layout "qiskit2e.qasm" "qiskit2e.layout" in
let (cb, lb) = read_circ_and_layout "qiskit2c.qasm" "qiskit2c.layout.error" in
let test3 = check_swap_equivalence ca cb la lb in

if not test1 && not test2 && not test3
then printf "\tPASSED\n"
else printf "\tFAILED\n";;

printf "Trying trivial layout\n";
let (c, l) = read_circ "qiskit1a.qasm" 7 in
let cg = make_lnn 7 in
let _ = printf "\tInitial 2q gates: %d\n" (count_2q c) in
let c' = swap_route c l cg in
let _ = printf "\tAfter routing 2q gates: %d\n" (count_2q c') in
if check_swap_equivalence c c' l l
then printf "\tEquivalence checks passed\n"
else printf "\tEquivalence checks failed\n";;

printf "Trying greedy layout\n";
let (c, trivl) = read_circ "qiskit1a.qasm" 7 in
let cg = make_lnn 7 in
let l = greedy_layout c cg in
let _ = printf "\tGenerated layout: " in
let _ = List.iter (printf "%d ") (layout_to_list l 7) in
let _ = printf "\n\tInitial 2q gates: %d\n" (count_2q c) in
let c' = swap_route c l cg in
let _ = printf "\tAfter routing 2q gates: %d\n" (count_2q c') in
if check_swap_equivalence c c' trivl l
then printf "\tEquivalence checks passed\n"
else printf "\tEquivalence checks failed\n";;
