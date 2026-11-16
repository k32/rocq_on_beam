(lang dune 3.23)
(using rocq 0.13)
(cram enable)

(package
  (name rocq_on_beam)
  (synopsis "Extraction of Rocq to Erlang")
  (depends
    ocaml
    (rocq-core (>= 9.4))))
