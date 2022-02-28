# Run t|ket> on all the benchmarks

# TODO: check that we're using the right optimization(s)

import pytket
from pytket.qasm import circuit_from_qasm
from pytket.passes import FullPeepholeOptimise
from pytket.circuit import OpType
import os
import sys
import time

def run(d,fname):
    
    f = open(fname, "w")
    
    f.write("name,1q,2q,total,time\n")
    
    for fname in os.listdir(d):

        print("Processing %s..." % fname)
        
        circ = circuit_from_qasm(os.path.join(d, fname))
        
        start = time.perf_counter()
        FullPeepholeOptimise().apply(circ)
        stop = time.perf_counter()

        total_count = circ.n_gates
        two_count = circ.n_gates_of_type(OpType.CX)
        one_count = total_count - two_count
        print("\t Total %d, 1q %d, CNOT %d\n" % (total_count, one_count, two_count))

        f.write("%s,%d,%d,%d,%f\n" % (fname, one_count, two_count, total_count, stop - start))
        
    f.close()

if (len(sys.argv) != 3):
    print("Usage: python run_tket.py <input directory> <output file>")
    exit(-1)

run(sys.argv[1], sys.argv[2])
