module Crc = struct
  (* The identifier is a CRC-32 over the definition with its incidental
     syntax removed. Everything that is not part of the type's identity --
     comments, grouping parentheses, an explicit identifier, the terminating
     semicolon, runs of whitespace -- is stripped first. *)
  let is_hex = function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

  (* Everything that is not part of the type's identity is removed, in order:
     a trailing comment, an explicit #id on the constructor name, grouping
     parentheses and the terminating semicolon, then runs of whitespace. *)
  let strip_comment s =
    let n = String.length s in
    let rec go i = if i + 1 >= n then s else if s.[i] = '/' && s.[i + 1] = '/' then String.sub s 0 i else go (i + 1) in
    go 0

  let strip_explicit_id s =
    let n = String.length s in
    match String.index_opt s '#' with
    | Some i when i > 0 ->
        let j = ref (i + 1) in
        while !j < n && is_hex s.[!j] do
          incr j
        done;
        (* Only a run of hex digits ending the constructor name counts. *)
        if !j > i + 1 && (!j >= n || s.[!j] = ' ' || s.[!j] = '\t' || s.[!j] = '=') then
          String.sub s 0 i ^ String.sub s !j (n - !j)
        else s
    | _ -> s

  let squeeze s =
    let b = Buffer.create (String.length s) in
    let pending_space = ref false in
    String.iter
      (fun c ->
        match c with
        | '(' | ')' | ';' -> ()
        | ' ' | '\t' | '\n' | '\r' -> if Buffer.length b > 0 then pending_space := true
        | c ->
            if !pending_space then Buffer.add_char b ' ';
            pending_space := false;
            Buffer.add_char b c)
      s;
    Buffer.contents b

  let normalize line = squeeze (strip_explicit_id (strip_comment line))

  let constructor_id line = Int32.of_int (Web3_codec.Crc.crc32 (normalize line) land 0xffffffff)

  (* A few constructors pin their identifier in the schema. Where that
     happens the written value is authoritative and differs from what the
     text would hash to, so it must not be recomputed. *)
  let explicit_id line =
    let line = strip_comment line in
    let n = String.length line in
    match String.index_opt line '#' with
    | Some i when i > 0 ->
        let j = ref (i + 1) in
        while !j < n && is_hex line.[!j] do
          incr j
        done;
        if !j - (i + 1) = 8 && (!j >= n || line.[!j] = ' ' || line.[!j] = '\t' || line.[!j] = '=') then
          Some (Int32.of_string ("0x" ^ String.sub line (i + 1) 8))
        else None
    | _ -> None

  let id_of_definition line =
    match explicit_id line with Some id -> id | None -> constructor_id line
end

module Reader = struct
  type t = { s : string; mutable pos : int }

  type error =
    | Truncated of { field : string; want : int; have : int }
    | Bad_constructor of { expected : int32; got : int32 }
    | Bad_length_prefix of int
    | Bad_padding
    | Bad_bool of int32
    | Message of string

  exception Error of error

  let pp_error ppf = function
    | Truncated { field; want; have } ->
        Format.fprintf ppf "truncated reading %s: want %d bytes, have %d" field want have
    | Bad_constructor { expected; got } ->
        Format.fprintf ppf "expected constructor %08lx, got %08lx" expected got
    | Bad_length_prefix n -> Format.fprintf ppf "invalid length prefix %d" n
    | Bad_padding -> Format.fprintf ppf "non-zero padding"
    | Bad_bool id -> Format.fprintf ppf "expected a boolean constructor, got %08lx" id
    | Message m -> Format.pp_print_string ppf m

  let fail e = raise (Error e)
  let make s = { s; pos = 0 }
  let parse s f = try Ok (f (make s)) with Error e -> Error e
  let remaining r = String.length r.s - r.pos
  let at_end r = remaining r = 0
  let finish r = if not (at_end r) then fail (Message (Printf.sprintf "%d trailing bytes" (remaining r)))

  let take r field n =
    if n < 0 || n > remaining r then fail (Truncated { field; want = n; have = remaining r });
    let v = String.sub r.s r.pos n in
    r.pos <- r.pos + n;
    v

  let u8 r field =
    if remaining r < 1 then fail (Truncated { field; want = 1; have = remaining r });
    let v = Char.code r.s.[r.pos] in
    r.pos <- r.pos + 1;
    v

  let le r field n =
    let s = take r field n in
    let v = ref 0L in
    for i = n - 1 downto 0 do
      v := Int64.logor (Int64.shift_left !v 8) (Int64.of_int (Char.code s.[i]))
    done;
    !v

  let int r = Int64.to_int32 (le r "int" 4)
  let nat r = int r
  let long r = le r "long" 8
  let double r = Int64.float_of_bits (le r "double" 8)
  let int128 r = take r "int128" 16
  let int256 r = take r "int256" 32

  (* Short form for anything under 254 bytes, then two escapes for longer
     values, and the whole field is padded so the total is a multiple of 4. *)
  let bytes r =
    let first = u8 r "bytes length" in
    let len, prefix =
      if first < 254 then (first, 1)
      else if first = 254 then (Int64.to_int (le r "bytes length" 3), 4)
      else
        let n = Int64.to_int (le r "bytes length" 4) in
        (* 0xFF is followed by a four-byte length and three zero bytes. *)
        let pad = take r "bytes length padding" 3 in
        if pad <> "\000\000\000" then fail Bad_padding;
        (n, 8)
    in
    if len < 0 then fail (Bad_length_prefix len);
    let data = take r "bytes" len in
    let pad = (4 - ((prefix + len) mod 4)) mod 4 in
    let padding = take r "bytes padding" pad in
    if String.exists (fun c -> c <> '\000') padding then fail Bad_padding;
    data

  let string = bytes
  let constructor r = int r
  let expect r id = let got = constructor r in if got <> id then fail (Bad_constructor { expected = id; got })

  (* Booleans are boxed: the value is entirely in the constructor. *)
  let bool_true = 0x997275b5l
  let bool_false = 0xbc799737l

  let bool r =
    let id = constructor r in
    if id = bool_true then true
    else if id = bool_false then false
    else fail (Bad_bool id)

  let vector r f =
    let n = Int32.to_int (nat r) in
    if n < 0 || n > remaining r then fail (Message (Printf.sprintf "implausible vector length %d" n));
    List.init n (fun _ -> f r)
end

module Writer = struct
  type t = Buffer.t

  let make () = Buffer.create 256
  let contents = Buffer.contents

  let to_string f =
    let b = make () in
    f b;
    contents b

  let le b v n =
    for i = 0 to n - 1 do
      Buffer.add_char b (Char.unsafe_chr (Int64.to_int (Int64.logand (Int64.shift_right_logical v (8 * i)) 0xffL)))
    done

  let int b v = le b (Int64.logand (Int64.of_int32 v) 0xffffffffL) 4
  let nat = int
  let long b v = le b v 8
  let double b v = le b (Int64.bits_of_float v) 8

  let fixed name n b s =
    if String.length s <> n then
      invalid_arg (Printf.sprintf "Tl.Writer.%s: expected %d bytes, got %d" name n (String.length s));
    Buffer.add_string b s

  let int128 = fixed "int128" 16
  let int256 = fixed "int256" 32

  let bytes b s =
    let len = String.length s in
    let prefix =
      if len < 254 then (
        Buffer.add_char b (Char.unsafe_chr len);
        1)
      else if len < 0x1000000 then (
        Buffer.add_char b '\254';
        le b (Int64.of_int len) 3;
        4)
      else (
        Buffer.add_char b '\255';
        le b (Int64.of_int len) 4;
        Buffer.add_string b "\000\000\000";
        8)
    in
    Buffer.add_string b s;
    (* Pad so the field as a whole occupies a multiple of four bytes. *)
    for _ = 1 to (4 - ((prefix + len) mod 4)) mod 4 do
      Buffer.add_char b '\000'
    done

  let string = bytes
  let constructor = int
  let bool b v = constructor b (if v then 0x997275b5l else 0xbc799737l)

  let vector b f l =
    nat b (Int32.of_int (List.length l));
    List.iter (f b) l
end
