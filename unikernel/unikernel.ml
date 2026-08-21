(* The MirageOS entry point. Deliberately thin: {!Light_client} holds the
   actual work and lives outside this file so that it type-checks in the
   ordinary build, not only under `mirage configure`. *)

(* Defaults, so the demo runs with no configuration. The first mainnet
   liteserver in TON's published global config; parameterising these is what
   Mirage's runtime arguments are for. *)
let liteserver_ip = "5.9.10.47"
let liteserver_port = 19949
let liteserver_key = "n4VDnSCUuSpjnCyUk9e3QOOd6o0ItSWYbTnW3Wnn8wk="

(* The elector, a masterchain account every node carries. Being on the
   masterchain, the block a query names is the block its proof is rooted at,
   so no shard link is involved. *)
let account = "-1:3333333333333333333333333333333333333333333333333333333333333333"

module Main (S : Tcpip.Stack.V4V6) = struct
  module LC = Light_client.Make (S)

  let start stack =
    let server_pub =
      match Base64.decode liteserver_key with
      | Ok k -> k
      | Error (`Msg m) -> failwith ("liteserver key is not base64: " ^ m)
    in
    LC.run stack ~host:(Ipaddr.of_string_exn liteserver_ip) ~port:liteserver_port ~server_pub ~account
end
