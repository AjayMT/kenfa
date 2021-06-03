(lang dune 2.6)
(name kenfa)

(generate_opam_files true)

(package
 (name kenfa)
 (synopsis "Regular expression library")
 (depends
  (dune (>= 2.6.0))
  (base (>= 0.14.1))
  (odoc :with-doc)))
