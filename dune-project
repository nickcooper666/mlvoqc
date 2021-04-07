(lang dune 2.0)
(name voqc)

(generate_opam_files true)

(source (github inQWIRE/mlvoqc))
(documentation "https://inQWIRE.github.io/voqc")
(license MIT)
(authors "Kesha Hietala")
(maintainers "kesha@cs.umd.edu")

(package
 (name voqc)
 (synopsis "A verified optimizer for quantum circuits (VOQC)")
 (description "OCaml library for VOQC, a Verified Optimizer for Quantum Circuits")
 (depends
  openQASM
  zarith))