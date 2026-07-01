(lang dune 3.21)

(name mal)

(generate_opam_files true)

(source
 (github silent-brad/mal))

(authors "Brad White")

(license LICENSE)

(package
 (name mal)
 (synopsis "Make a Lisp in OCaml")
 (depends ocaml lisp mal))
