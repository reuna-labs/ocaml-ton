(* Developer tool: report how a Bag of Cells is encoded and whether it
   survives a decode/encode round-trip. Not part of the library. *)
open Ton_cell

let hex s = String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i])))

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic; s

let load p =
  let raw = read_file p in
  if String.length raw > 3 && String.sub raw 0 3 = "te6" then
    match Base64.decode (String.concat "" (List.map String.trim (String.split_on_char '\n' raw))) with
    | Ok s -> s
    | Error (`Msg m) -> failwith m
  else raw

let survey root =
  let seen = Hashtbl.create 64 in
  let n = ref 0 and exotic = ref 0 and lvl = ref 0 in
  let rec go c =
    let h = Cell.identity c in
    if not (Hashtbl.mem seen h) then begin
      Hashtbl.add seen h (); incr n;
      if Cell.is_exotic c then incr exotic;
      lvl := max !lvl (Cell.level c);
      List.iter go (Cell.refs c)
    end
  in go root; (!n, !exotic, !lvl)

let () =
  Array.iteri (fun i p ->
    if i > 0 then begin
      let src = load p in
      let b k = Char.code src.[k] in
      let magic = (b 0 lsl 24) lor (b 1 lsl 16) lor (b 2 lsl 8) lor b 3 in
      Printf.printf "\n=== %s (%d bytes)\n" (Filename.basename p) (String.length src);
      Printf.printf "magic=%08x" magic;
      if magic = 0xb5ee9c72 then begin
        let f = b 4 in
        Printf.printf " flags=%02x idx=%b crc=%b cache=%b size=%d off_bytes=%d"
          f (f land 0x80 <> 0) (f land 0x40 <> 0) (f land 0x20 <> 0) (f land 7) (b 5)
      end;
      print_newline ();
      (* does any cell store hashes inline? *)
      (match Boc.deserialize src with
       | Error e -> Format.printf "PARSE ERROR: %a@." Boc.pp_error e
       | Ok roots ->
         Printf.printf "roots=%d\n" (List.length roots);
         List.iteri (fun j r ->
           let n, ex, lvl = survey r in
           Printf.printf "  root[%d] hash=%s cells=%d exotic=%d maxlevel=%d depth=%d\n"
             j (hex (Cell.hash r)) n ex lvl (Cell.depth r)) roots;
         match roots with
         | [ root ] ->
           List.iter (fun (idx, crc) ->
             let out = Boc.serialize ~idx ~crc32:crc root in
             match Boc.deserialize_root out with
             | Error e -> Format.printf "  re-encode idx=%b crc=%b -> ERROR %a@." idx crc Boc.pp_error e
             | Ok back ->
               Printf.printf "  re-encode idx=%b crc=%b -> %s (%d bytes)%s\n" idx crc
                 (if String.equal (Cell.hash back) (Cell.hash root) then "hash ok" else "HASH MISMATCH")
                 (String.length out)
                 (if String.equal out src then " [byte-identical]" else ""))
             [ (false,false); (true,false); (false,true); (true,true) ]
         | _ -> print_endline "  (multi-root: re-encode not attempted)")
    end) Sys.argv
