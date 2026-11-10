(lang dune 3.23)
(using rocq 0.13)

(package
  (name rocq-core-erlang)
  (synopsis "Rocq extraction plugin targeting Core Erlang")
  (depends
    ocaml
    (rocq-core (>= 9.0))
    coq-libtx-storage))