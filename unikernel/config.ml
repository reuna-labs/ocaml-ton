open Mirage

let packages =
  [ package "ton-cell"; package "ton-tlb"; package "ton-address"; package "ton-proof";
    package "ton-lite-client"; package "ton-lite-client-mirage"; package "ton-light-client";
    package "base64"; package "mirage-crypto-rng-mirage" ]

let main = main "Unikernel.Main" ~packages (stackv4v6 @-> job)
let () = register "ton-light-client" [ main $ generic_stackv4v6 default_network ]
