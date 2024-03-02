(lang dune 2.7)

(name jsonoo)

(license MIT)

(maintainers "Max Lantas <mnxndev@outlook.com>")

(authors "Max Lantas <mnxndev@outlook.com>")

(source
 (github mnxn/jsonoo))

(documentation https://mnxn.github.io/jsonoo/)

(generate_opam_files true)

(implicit_transitive_deps false)

(package
 (name jsonoo)
 (synopsis "JSON library for Js_of_ocaml")
 (depends
  (ocaml
   (>= 4.08))
  js_of_ocaml
  js_of_ocaml-ppx
  (gen_js_api
   (>= 1.0.8))
  (webtest :with-test)
  (webtest-js :with-test)
  (conf-npm :with-test)))
