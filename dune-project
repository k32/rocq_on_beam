(lang dune 3.23)
(name coreerlang)
(using rocq 0.13)

(package
  (name coreerlang)
  (depends
    ocaml
    (rocq-core (>= 9.0))
    coq-libtx-storage))