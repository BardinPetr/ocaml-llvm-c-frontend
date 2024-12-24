(lang dune 3.16)

(name lab4)

(generate_opam_files true)

(source
 (github BardinPetr/ocaml-c-frontend))

(authors "Bardin Petr")

(maintainers "Bardin Petr")

(license MIT)

(package
 (name lab4)
 (synopsis "ocaml-c-frontend")
 (description "ITMO FP course laboratory work #4")
 (depends ocaml dune (alcotest :with-test) (junit_alcotest :with-test) (qcheck :with-test) (qcheck-alcotest :with-test) (ppx_inline_test :with-test) (ppx_expect :with-test) ppx_jane ppx_deriving ppx_import cmdliner menhir llvm)
 (allow_empty))

(using menhir 3.0)
