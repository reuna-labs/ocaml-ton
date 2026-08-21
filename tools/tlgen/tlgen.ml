(* A compiler for the subset of TL that TON's schemas actually use.

   Deliberately narrow: it handles exactly the constructs present in
   lite_api.tl and the ADNL slice of ton_api.tl, and fails loudly on anything
   else rather than guessing. The point is not to implement TL, it is to stop
   ninety constructor identifiers being transcribed by hand -- a wrong one is
   not a type error, it is a liteserver hanging up without a word. *)

let die fmt = Printf.ksprintf (fun s -> prerr_endline ("tlgen: " ^ s); exit 1) fmt

(* --- schema model ---------------------------------------------------------- *)

type ty =
  | Int
  | Nat                     (* # -- a flags word rather than a number *)
  | Long
  | Double
  | Bytes
  | String
  | Int128
  | Int256
  | Bool
  | True                    (* zero width; only ever under a conditional *)
  | Vector of ty
  | Bare of string          (* another constructor, written without its id *)
  | Boxed of string         (* a result type, written with its id *)
  | Cond of string * int * ty  (* present iff bit N of the named field is set *)

type field = { fname : string; fty : ty }

type ctor = {
  name : string;
  id : int32;
  explicit : bool;
  fields : field list;
  result : string;
  is_function : bool;
  source : string;
}

(* --- lexing ---------------------------------------------------------------- *)

let strip_comment s =
  let n = String.length s in
  let rec go i = if i + 1 >= n then s else if s.[i] = '/' && s.[i + 1] = '/' then String.sub s 0 i else go (i + 1) in
  go 0

(* Definitions run to a semicolon and three of them wrap across lines, so the
   file is read as a stream of statements rather than a list of lines. *)
let statements path =
  let ic = open_in path in
  let out = ref [] and buf = Buffer.create 256 and functions = ref false in
  (try
     while true do
       let line = strip_comment (input_line ic) in
       let t = String.trim line in
       if t = "---functions---" then functions := true
       else if t = "---types---" then functions := false
       else if t <> "" then begin
         Buffer.add_string buf line;
         Buffer.add_char buf ' ';
         if String.contains t ';' then begin
           out := (String.trim (Buffer.contents buf), !functions) :: !out;
           Buffer.clear buf
         end
       end
     done
   with End_of_file -> close_in ic);
  if String.trim (Buffer.contents buf) <> "" then die "unterminated definition: %s" (Buffer.contents buf);
  List.rev !out

(* Split on whitespace, keeping parenthesised groups together. *)
let tokens s =
  let out = ref [] and buf = Buffer.create 32 and depth = ref 0 in
  let flush () = if Buffer.length buf > 0 then (out := Buffer.contents buf :: !out; Buffer.clear buf) in
  String.iter
    (fun c ->
      match c with
      | '(' -> incr depth; Buffer.add_char buf c
      | ')' -> decr depth; Buffer.add_char buf c
      | ' ' | '\t' when !depth = 0 -> flush ()
      | c -> Buffer.add_char buf c)
    s;
  flush ();
  List.rev !out

(* --- parsing --------------------------------------------------------------- *)

let builtins =
  [ "int"; "long"; "double"; "string"; "object"; "function"; "bytes"; "true";
    "boolTrue"; "boolFalse"; "vector"; "int128"; "int256" ]

let starts_upper s =
  match String.rindex_opt s '.' with
  | Some i when i + 1 < String.length s -> s.[i + 1] >= 'A' && s.[i + 1] <= 'Z'
  | _ -> String.length s > 0 && s.[0] >= 'A' && s.[0] <= 'Z'

let rec parse_ty ctx t =
  if String.length t > 0 && t.[0] = '(' then begin
    let inner = String.sub t 1 (String.length t - 2) in
    match tokens inner with
    | [ "vector"; e ] -> Vector (parse_ty ctx e)
    | [ e ] -> parse_ty ctx e (* parentheses used purely for grouping *)
    | _ -> die "%s: unsupported parenthesised type %s" ctx t
  end
  else
    match String.index_opt t '?' with
    | Some q ->
        (* mode.N?type *)
        let cond = String.sub t 0 q and rest = String.sub t (q + 1) (String.length t - q - 1) in
        let dot = match String.index_opt cond '.' with Some d -> d | None -> die "%s: malformed condition %s" ctx t in
        let fld = String.sub cond 0 dot in
        let bit = int_of_string (String.sub cond (dot + 1) (String.length cond - dot - 1)) in
        Cond (fld, bit, parse_ty ctx rest)
    | None -> (
        match t with
        | "int" -> Int
        | "#" -> Nat
        | "long" -> Long
        | "double" -> Double
        | "bytes" -> Bytes
        | "string" -> String
        | "int128" -> Int128
        | "int256" -> Int256
        | "Bool" -> Bool
        | "true" -> True
        | _ when starts_upper t -> Boxed t
        | _ -> Bare t)

let head_name src =
  let src = String.trim src in
  let n = String.length src in
  let rec go i = if i >= n || src.[i] = ' ' || src.[i] = '\t' || src.[i] = '#' || src.[i] = '=' then String.sub src 0 i else go (i + 1) in
  go 0

let parse ?(keep = fun _ -> true) (src, is_function) =
  if not (keep (head_name src)) then None else
  let body = String.trim (String.concat "" (String.split_on_char ';' src)) in
  match String.index_opt body '=' with
  | None -> die "definition has no result type: %s" src
  | Some eq ->
      let lhs = String.trim (String.sub body 0 eq) in
      let result = String.trim (String.sub body (eq + 1) (String.length body - eq - 1)) in
      let ts = tokens lhs in
      let head, rest = match ts with h :: r -> (h, r) | [] -> die "empty definition: %s" src in
      let name = match String.index_opt head '#' with Some i -> String.sub head 0 i | None -> head in
      if List.mem name builtins then None
      else begin
        let fields =
          List.filter_map
            (fun t ->
              if String.length t > 0 && t.[0] = '{' then None (* type variable binder *)
              else
                match String.index_opt t ':' with
                | None -> die "%s: field without a type: %s" name t
                | Some i ->
                    Some { fname = String.sub t 0 i; fty = parse_ty name (String.sub t (i + 1) (String.length t - i - 1)) })
            rest
        in
        Some
          { name;
            id = Ton_tl.Tl.Crc.id_of_definition src;
            explicit = Ton_tl.Tl.Crc.explicit_id src <> None;
            fields; result; is_function; source = String.trim src }
      end

(* --- naming ---------------------------------------------------------------- *)

let snake s =
  let b = Buffer.create (String.length s + 8) in
  String.iteri
    (fun i c ->
      if c = '.' then Buffer.add_char b '_'
      else if c >= 'A' && c <= 'Z' then begin
        if i > 0 && s.[i - 1] <> '.' then Buffer.add_char b '_';
        Buffer.add_char b (Char.lowercase_ascii c)
      end
      else Buffer.add_char b c)
    s;
  Buffer.contents b

let keywords =
  [ "and"; "as"; "assert"; "begin"; "class"; "constraint"; "do"; "done"; "downto"; "else"; "end";
    "exception"; "external"; "false"; "for"; "fun"; "function"; "functor"; "if"; "in"; "include";
    "inherit"; "initializer"; "land"; "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method";
    "mod"; "module"; "mutable"; "new"; "nonrec"; "object"; "of"; "open"; "or"; "private"; "rec";
    "sig"; "struct"; "then"; "to"; "true"; "try"; "type"; "val"; "virtual"; "when"; "while"; "with" ]

let ident s = let s = snake s in if List.mem s keywords then s ^ "_" else s

(* --- code generation -------------------------------------------------------- *)

let buf = Buffer.create (1 lsl 16)
let p fmt = Printf.ksprintf (Buffer.add_string buf) fmt

let rec ocaml_ty = function
  | Int | Nat -> "int32"
  | Long -> "int64"
  | Double -> "float"
  | Bytes | String | Int128 | Int256 -> "string"
  | Bool -> "bool"
  | True -> "bool"
  | Vector t -> ocaml_ty t ^ " list"
  | Bare n | Boxed n -> ident n
  | Cond (_, _, True) -> "bool"
  | Cond (_, _, t) -> ocaml_ty t ^ " option"

let rec reader = function
  | Int -> "R.int r"
  | Nat -> "R.nat r"
  | Long -> "R.long r"
  | Double -> "R.double r"
  | Bytes -> "R.bytes r"
  | String -> "R.string r"
  | Int128 -> "R.int128 r"
  | Int256 -> "R.int256 r"
  | Bool -> "R.bool r"
  | True -> "true"
  | Vector t -> Printf.sprintf "R.vector r (fun r -> %s)" (reader t)
  | Bare n -> Printf.sprintf "read_%s r" (ident n)
  | Boxed n -> Printf.sprintf "read_boxed_%s r" (ident n)
  | Cond (f, bit, True) -> Printf.sprintf "bit %s %d" (ident f) bit
  | Cond (f, bit, t) -> Printf.sprintf "if bit %s %d then Some (%s) else None" (ident f) bit (reader t)

let rec writer v = function
  | Int -> Printf.sprintf "W.int w %s" v
  | Nat -> Printf.sprintf "W.nat w %s" v
  | Long -> Printf.sprintf "W.long w %s" v
  | Double -> Printf.sprintf "W.double w %s" v
  | Bytes -> Printf.sprintf "W.bytes w %s" v
  | String -> Printf.sprintf "W.string w %s" v
  | Int128 -> Printf.sprintf "W.int128 w %s" v
  | Int256 -> Printf.sprintf "W.int256 w %s" v
  | Bool -> Printf.sprintf "W.bool w %s" v
  | True -> "()"
  | Vector t -> Printf.sprintf "W.vector w (fun w v -> %s) %s" (writer "v" t) v
  | Bare n -> Printf.sprintf "write_%s w %s" (ident n) v
  | Boxed n -> Printf.sprintf "write_boxed_%s w %s" (ident n) v
  | Cond (f, bit, True) ->
      Printf.sprintf "check_flag %S %d (bit %s %d) %s" f bit (ident f) bit v
  | Cond (f, bit, t) ->
      Printf.sprintf
        "(check_flag %S %d (bit %s %d) (%s <> None); match %s with Some v -> %s | None -> ())" f bit
        (ident f) bit v v (writer "v" t)

(* Records and the variants over them refer to each other in whichever order
   the schema happens to list them, so everything is emitted as one mutually
   recursive group rather than sorted. *)
let type_body c =
  if c.fields = [] then Printf.sprintf "%s = unit" (ident c.name)
  else
    Printf.sprintf "%s = {\n%s\n}" (ident c.name)
      (String.concat "\n" (List.map (fun f -> Printf.sprintf "  %s : %s;" (ident f.fname) (ocaml_ty f.fty)) c.fields))

let fmt = Printf.sprintf

(* Each entry is a binding body, joined into one `let rec ... and ...` chain
   by the caller for the same reason the types are. *)
let fun_bodies c =
  let n = ident c.name in
  let read =
    (* The return type is written out because many constructors share field
       names; without it OCaml would resolve the record literal to whichever
       type defined those labels last. *)
    if c.fields = [] then fmt "read_%s (_ : R.t) : %s = ()" n n
    else
      fmt "read_%s r : %s =\n%s\n  { %s }" n n
        (String.concat "\n" (List.map (fun f -> fmt "  let %s = %s in" (ident f.fname) (reader f.fty)) c.fields))
        (String.concat "; " (List.map (fun f -> ident f.fname) c.fields))
  in
  let write =
    if c.fields = [] then fmt "write_%s (_ : W.t) (_ : %s) = ()" n n
    else
      fmt "write_%s w (v : %s) =\n%s\n%s\n  ()" n n
        (String.concat "\n" (List.map (fun f -> fmt "  let %s = v.%s in" (ident f.fname) (ident f.fname)) c.fields))
        (String.concat "\n" (List.map (fun f -> fmt "  %s;" (writer (ident f.fname) f.fty)) c.fields))
  in
  [ read; write;
    fmt "read_boxed_%s r : %s = R.expect r %s_id; read_%s r" n n n n;
    fmt "write_boxed_%s w v = W.constructor w %s_id; write_%s w v" n n n ]

let () =
  let schema = ref "" and out = ref "" and only = ref [] in
  let args = Array.to_list Sys.argv in
  let rec parse_args = function
    | [] -> ()
    | "--schema" :: v :: rest -> schema := v; parse_args rest
    | "-o" :: v :: rest -> out := v; parse_args rest
    | "--only" :: v :: rest -> only := String.split_on_char ',' v; parse_args rest
    | a :: rest -> if !schema = "" then schema := a else die "unexpected argument %s" a; parse_args rest
  in
  parse_args (List.tl args);
  if !schema = "" then die "usage: tlgen --schema FILE [-o OUT] [--only prefix,prefix]";
  (* Filter before parsing, not after: the schema contains constructs this
     compiler deliberately does not understand, and they should only be an
     error if they are actually wanted. *)
  let keep name = !only = [] || List.exists (fun pfx -> String.starts_with ~prefix:pfx name) !only in
  let ctors = List.filter_map (parse ~keep) (statements !schema) in
  (* Result types with more than one constructor become variants; with exactly
     one, the constructor's own record stands in for the type. *)
  let by_result = Hashtbl.create 64 in
  List.iter
    (fun c ->
      if not c.is_function then
        Hashtbl.replace by_result c.result (c :: (try Hashtbl.find by_result c.result with Not_found -> [])))
    ctors;
  let builtin_results = [ "Object"; "Bool"; "True"; "Int"; "Long"; "Double"; "String"; "Bytes"; "Int128"; "Int256"; "Function" ] in
  let variants =
    Hashtbl.fold
      (fun rt cs acc -> if List.length cs > 1 && not (List.mem rt builtin_results) then (rt, List.rev cs) :: acc else acc)
      by_result []
  in
  let variants = List.sort (fun (a, _) (b, _) -> compare a b) variants in
  let variant_case rt c =
    let rtn = ident rt ^ "_" and cn = ident c.name in
    let short =
      if String.starts_with ~prefix:rtn cn then String.sub cn (String.length rtn) (String.length cn - String.length rtn)
      else cn
    in
    String.capitalize_ascii short
  in
  p "(* Generated by tools/tlgen from %s. Do not edit.\n\n" (Filename.basename !schema);
  p "   Constructor identifiers are computed from the schema text rather than\n";
  p "   transcribed, except where the schema pins one explicitly. *)\n\n";
  p "[@@@warning \"-30-32-34-37-39\"]\n\n";
  p "module R = Ton_tl.Tl.Reader\nmodule W = Ton_tl.Tl.Writer\n\n";
  p "let bit flags n = Int32.logand flags (Int32.shift_left 1l n) <> 0l\n\n";
  p "(* A conditional field and its flag bit have to agree, or the field is\n";
  p "   silently dropped on the wire. Catch that here rather than at the peer. *)\n";
  p "let check_flag name n set present =\n";
  p "  if set <> present then\n";
  p "    invalid_arg\n";
  p "      (Printf.sprintf \"%%s.%%d is %%b but the field is %%b\" name n set present)\n\n";
  (* types *)
  let type_bodies =
    List.map type_body ctors
    @ List.map
        (fun (rt, cs) ->
          Printf.sprintf "%s =\n%s" (ident rt)
            (String.concat "\n" (List.map (fun c -> Printf.sprintf "  | %s of %s" (variant_case rt c) (ident c.name)) cs)))
        variants
  in
  List.iteri (fun i b -> p "%s %s\n\n" (if i = 0 then "type" else "and") b) type_bodies;
  (* identifiers, with the schema line that produced each one *)
  List.iter (fun c -> p "(* %s *)\nlet %s_id = 0x%08lxl%s\n\n" c.source (ident c.name) c.id
                (if c.explicit then "  (* pinned by the schema *)" else "")) ctors;
  (* readers and writers *)
  let bodies =
    List.concat_map fun_bodies ctors
    @ List.concat_map
        (fun (rt, cs) ->
          let n = ident rt in
          [ Printf.sprintf "read_boxed_%s r : %s =\n  let id = R.constructor r in\n%s\n  else R.fail (R.Message (Printf.sprintf \"unexpected constructor %%08lx for %s\" id))"
              n n
              (String.concat "\n"
                 (List.mapi
                    (fun i c ->
                      Printf.sprintf "  %s id = %s_id then %s (read_%s r)" (if i = 0 then "if" else "else if")
                        (ident c.name) (variant_case rt c) (ident c.name))
                    cs))
              rt;
            Printf.sprintf "write_boxed_%s w = function\n%s" n
              (String.concat "\n"
                 (List.map
                    (fun c ->
                      Printf.sprintf "  | %s v -> W.constructor w %s_id; write_%s w v" (variant_case rt c)
                        (ident c.name) (ident c.name))
                    cs)) ])
        variants
  in
  List.iteri (fun i b -> p "%s %s\n\n" (if i = 0 then "let rec" else "and") b) bodies;
  let oc = if !out = "" then stdout else open_out !out in
  output_string oc (Buffer.contents buf);
  if !out <> "" then close_out oc;
  Printf.eprintf "tlgen: %d constructors from %s\n" (List.length ctors) (Filename.basename !schema)
