(lang dune 3.4)

(name irmin-ipfs)

(generate_opam_files true)

(source
 (github metanivek/irmin-ipfs))

(authors "metanivek")

(maintainers "metanivek")

(license 0BSD)

(package
 (name irmin-ipfs)
 (synopsis "IPFS storage for Irmin")
 (description "IPFS storage for Irmin")
 (depends ocaml dune (irmin (>= :3.4.1)))
 (tags
  (irmin ipfs)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
