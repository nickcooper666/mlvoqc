# Run PyZX on all the benchmarks
# You will need to run this from the directory where PyZX is installed

import pyzx as zx
import os
import sys
import time

def run(d, fname):
    
    f = open(fname, "w")
        
    f.write("name,T,2q,total,time\n")
    
    for fname in os.listdir(d):

        print("Processing %s..." % fname)
        
        circ = zx.Circuit.load(os.path.join(d, fname)).to_basic_gates()

        start = time.perf_counter()
        g = circ.to_graph()
        zx.full_reduce(g,quiet=False)
        g.normalize()
        new_circ = zx.extract_circuit(g).to_basic_gates()
        stop = time.perf_counter()

        total_count = len(new_circ.gates)
        two_count = new_circ.twoqubitcount()
        t_count = zx.tcount(new_circ)

        print("\t Total %d, T %d, CNOT %d\n" % (total_count, t_count, two_count))

        f.write("%s,%d,%d,%d,%f\n" % (fname, t_count, two_count, total_count, stop - start))

    f.close()

if (len(sys.argv) != 3):
    print("Usage: python run_pyzx.py <input directory> <output file>")
    exit(-1)

run(sys.argv[1], sys.argv[2])
