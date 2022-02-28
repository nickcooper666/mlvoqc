#!/bin/bash

# Run VOQC on the benchmarks in Arithmetic_and_Toffoli_partial and save the 
# optimized circuits in results. Prints gate counts directly to the console.

filenames=( $(ls -d VOQC-benchmarks/Arithmetic_and_Toffoli_partial/*.qasm) )

mkdir -p results

for filename in "${filenames[@]}"
do
    program_name=`basename "$filename"`
    dune exec --root .. -- ./voqc_cli.exe -i ${filename} -o results/${program_name} -optimize-nam -optimize-ibm
done