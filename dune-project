(lang dune 3.0)
(name ton)
(generate_opam_files true)
(source (github reuna-labs/ocaml-ton))
(license ISC)
(authors "Gil Bahat <bahat.gil@gmail.com>")
(maintainers "Gil Bahat <bahat.gil@gmail.com>")

(package
 (name ton-cell)
 (synopsis "TON cells and Bag-of-Cells codec (pure OCaml, unikernel-safe)")
 (description
  "Bit-addressed cells, the representation hash (including exotic cells and\nMerkle proofs), and the Bag-of-Cells container format. No OS or IO\ndependencies, so it links into a MirageOS/Solo5 unikernel.")
 (depends
  (ocaml (>= 4.14))
  (digestif (>= 1.1.4))
  (zarith (>= 1.12))
  web3-codec
  (alcotest :with-test)
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)))

(package
 (name ton-address)
 (synopsis "TON address parsing and formatting")
 (description
  "Raw and user-friendly TON addresses, including bounceable, non-bounceable\nand testnet tags, CRC-16/XMODEM validation and both base64 alphabets.")
 (depends
  (ocaml (>= 4.14))
  web3-codec
  (base64 (>= 3.5.0))
  (alcotest :with-test)))
