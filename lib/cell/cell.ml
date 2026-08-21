(* Cell construction, exotic-cell validation and the representation hash all
   live in one module because they are mutually recursive: resolving a cell's
   level mask requires parsing its exotic payload, and validating a Merkle
   proof requires the hashes of the cells it references.

   The hash calculation is a direct transliteration of ton-core's
   [wonderCalculator]/[getRepr]/[LevelMask], which in turn replicate
   DataCell.cpp in ton-blockchain/ton. It is deliberately kept structurally
   close to the reference so the two can be diffed by eye; the TON
   documentation does not fully specify this logic. *)

let max_bits = 1023
let max_refs = 4
let max_depth = 1024

type t = {
  typ : Cell_type.t;
  bits : Bits.t;
  refs : t array;
  mask : Level_mask.t;
  (* Always 4 entries, indexed by level, so [hash]/[depth] are direct lookups. *)
  hashes : string array;
  depths : int array;
}

type error =
  | Too_many_bits of int
  | Too_many_refs of int
  | Depth_overflow of int
  | Exotic_too_short of int
  | Exotic_unknown_type of int
  | Pruned_has_refs of int
  | Pruned_bad_level of int
  | Pruned_bad_size of { expected : int; got : int }
  | Merkle_proof_bad_size of int
  | Merkle_proof_bad_refs of int
  | Merkle_update_bad_size of int
  | Merkle_update_bad_refs of int
  | Merkle_hash_mismatch of int
  | Merkle_depth_mismatch of int
  | Library_bad_size of int
  | Invalid_hash_layout

let pp_error ppf = function
  | Too_many_bits n -> Format.fprintf ppf "cell has %d bits, maximum is %d" n max_bits
  | Too_many_refs n -> Format.fprintf ppf "cell has %d refs, maximum is %d" n max_refs
  | Depth_overflow n -> Format.fprintf ppf "cell depth %d exceeds maximum %d" n max_depth
  | Exotic_too_short n -> Format.fprintf ppf "exotic cell has %d bits, need at least 8 for the type tag" n
  | Exotic_unknown_type n -> Format.fprintf ppf "unknown exotic cell type %d" n
  | Pruned_has_refs n -> Format.fprintf ppf "pruned branch must have no refs, got %d" n
  | Pruned_bad_level n -> Format.fprintf ppf "pruned branch level must be 1..3, got %d" n
  | Pruned_bad_size { expected; got } ->
      Format.fprintf ppf "pruned branch must have exactly %d bits, got %d" expected got
  | Merkle_proof_bad_size n -> Format.fprintf ppf "merkle proof must have exactly 280 bits, got %d" n
  | Merkle_proof_bad_refs n -> Format.fprintf ppf "merkle proof must have exactly 1 ref, got %d" n
  | Merkle_update_bad_size n -> Format.fprintf ppf "merkle update must have exactly 552 bits, got %d" n
  | Merkle_update_bad_refs n -> Format.fprintf ppf "merkle update must have exactly 2 refs, got %d" n
  | Merkle_hash_mismatch i -> Format.fprintf ppf "merkle cell stored hash %d does not match its ref" i
  | Merkle_depth_mismatch i -> Format.fprintf ppf "merkle cell stored depth %d does not match its ref" i
  | Library_bad_size n -> Format.fprintf ppf "library cell must have exactly 264 bits, got %d" n
  | Invalid_hash_layout -> Format.fprintf ppf "inconsistent level/type while computing cell hashes"

let ( let* ) = Result.bind
let sha256 s = Digestif.SHA256.(to_raw_string (digest_string s))

(* --- accessors used during hashing (before [t] is fully built) ------------ *)

let hash_at c level = c.hashes.(level)
let depth_at c level = c.depths.(level)

(* --- descriptors --------------------------------------------------------- *)

(* d1 = refs + 8*exotic + 32*mask. Bounded by 4 + 8 + 224 = 236, so it always
   fits in a byte. *)
let refs_descriptor ~nrefs ~mask ~typ =
  nrefs + (if Cell_type.is_exotic typ then 8 else 0) + (mask * 32)

(* d2 = ceil(bits/8) + floor(bits/8). Odd exactly when the data is not
   byte-aligned, which is the only signal that a completion tag is present.
   Bounded by 128 + 127 = 255. *)
let bits_descriptor b =
  let len = Bits.length b in
  ((len + 7) / 8) + (len / 8)

(* The byte string that gets hashed. Note the three different bit lengths in
   play: d1 uses the *masked* level, d2 uses the *original* bit length even at
   higher levels, and the body uses [cur_bits], which is the original data at
   level 0 but the previous level's hash above that. *)
let repr ~original_bits ~cur_bits ~refs ~level ~mask ~typ =
  let nrefs = Array.length refs in
  let buf = Buffer.create (2 + ((Bits.length cur_bits + 7) / 8) + (34 * nrefs)) in
  Buffer.add_char buf (Char.chr (refs_descriptor ~nrefs ~mask ~typ));
  Buffer.add_char buf (Char.chr (bits_descriptor original_bits));
  Buffer.add_string buf (Bits.to_padded_bytes cur_bits);
  (* Merkle proofs and updates reach one level further down into their
     children than ordinary cells do. *)
  let child_level =
    match typ with Cell_type.Merkle_proof | Cell_type.Merkle_update -> level + 1 | _ -> level
  in
  (* All depths first, then all hashes -- not interleaved. *)
  Array.iter
    (fun c ->
      let d = depth_at c child_level in
      Buffer.add_char buf (Char.unsafe_chr (d lsr 8));
      Buffer.add_char buf (Char.unsafe_chr (d land 0xff)))
    refs;
  Array.iter (fun c -> Buffer.add_string buf (hash_at c child_level)) refs;
  Buffer.contents buf

(* --- exotic layouts ------------------------------------------------------- *)

type pruned_entry = { p_hash : string; p_depth : int }

let u8 b pos = Int64.to_int (Bits.get_uint b ~pos ~len:8)
let u16 b pos = Int64.to_int (Bits.get_uint b ~pos ~len:16)
let hash256 b pos = Option.get (Bits.to_bytes (Bits.sub b pos 256))

let exotic_pruned bits refs =
  let n = Array.length refs in
  if n <> 0 then Error (Pruned_has_refs n)
  else
    let len = Bits.length bits in
    (* A real config proof observed on mainnet omits the mask byte; ton-core
       special-cases the resulting 280-bit form and pins the mask to 1. Keeping
       the quirk because rejecting it would make us unable to parse live data. *)
    let* m, data_off =
      if len = 280 then Ok (Level_mask.v 1, 8)
      else begin
        let m = Level_mask.v (u8 bits 8) in
        let lvl = Level_mask.level m in
        if lvl < 1 || lvl > 3 then Error (Pruned_bad_level lvl)
        else
          let expected =
            8 + 8 + (Level_mask.hash_count (Level_mask.apply m (lvl - 1)) * (256 + 16))
          in
          if len <> expected then Error (Pruned_bad_size { expected; got = len })
          else Ok (m, 16)
      end
    in
    let lvl = Level_mask.level m in
    let entries =
      Array.init lvl (fun i ->
          { p_hash = hash256 bits (data_off + (i * 256));
            p_depth = u16 bits (data_off + (lvl * 256) + (i * 16)) })
    in
    Ok (m, entries)

let exotic_merkle_proof bits refs =
  let len = Bits.length bits and n = Array.length refs in
  if len <> 8 + 256 + 16 then Error (Merkle_proof_bad_size len)
  else if n <> 1 then Error (Merkle_proof_bad_refs n)
  else if u16 bits 264 <> depth_at refs.(0) 0 then Error (Merkle_depth_mismatch 0)
  else if not (String.equal (hash256 bits 8) (hash_at refs.(0) 0)) then
    Error (Merkle_hash_mismatch 0)
  else Ok ()

let exotic_merkle_update bits refs =
  let len = Bits.length bits and n = Array.length refs in
  if len <> 8 + (2 * (256 + 16)) then Error (Merkle_update_bad_size len)
  else if n <> 2 then Error (Merkle_update_bad_refs n)
  else
    let rec check i =
      if i > 1 then Ok ()
      else if u16 bits (520 + (i * 16)) <> depth_at refs.(i) 0 then Error (Merkle_depth_mismatch i)
      else if not (String.equal (hash256 bits (8 + (i * 256))) (hash_at refs.(i) 0)) then
        Error (Merkle_hash_mismatch i)
      else check (i + 1)
    in
    check 0

let exotic_library bits =
  let len = Bits.length bits in
  if len <> 8 + 256 then Error (Library_bad_size len) else Ok ()

(* --- the hash/depth/level fixpoint ---------------------------------------- *)

exception Fail of error

let wonder typ bits refs =
  let* m, pruned =
    match typ with
    | Cell_type.Ordinary ->
        let m = Array.fold_left (fun acc r -> acc lor Level_mask.value r.mask) 0 refs in
        Ok (Level_mask.v m, None)
    | Cell_type.Pruned_branch ->
        let* m, entries = exotic_pruned bits refs in
        Ok (m, Some entries)
    | Cell_type.Merkle_proof ->
        let* () = exotic_merkle_proof bits refs in
        Ok (Level_mask.v (Level_mask.value refs.(0).mask lsr 1), None)
    | Cell_type.Merkle_update ->
        let* () = exotic_merkle_update bits refs in
        Ok
          ( Level_mask.v
              ((Level_mask.value refs.(0).mask lor Level_mask.value refs.(1).mask) lsr 1),
            None )
    | Cell_type.Library ->
        let* () = exotic_library bits in
        Ok (Level_mask.v 0, None)
  in
  let total_hash_count = Level_mask.hash_count m in
  (* A pruned branch computes only its own top hash; the lower ones are read
     out of the table stored in its data. *)
  let hash_count = if typ = Cell_type.Pruned_branch then 1 else total_hash_count in
  let hash_i_offset = total_hash_count - hash_count in
  let hashes = Array.make hash_count "" and depths = Array.make hash_count 0 in
  try
    let hash_i = ref 0 in
    for level_i = 0 to Level_mask.level m do
      if Level_mask.is_significant m level_i then
        if !hash_i < hash_i_offset then incr hash_i
        else begin
          let dest = !hash_i - hash_i_offset in
          let cur_bits =
            if dest = 0 then begin
              if not (level_i = 0 || typ = Cell_type.Pruned_branch) then raise (Fail Invalid_hash_layout);
              bits
            end
            else begin
              if not (level_i <> 0 && typ <> Cell_type.Pruned_branch) then
                raise (Fail Invalid_hash_layout);
              Bits.of_bytes hashes.(dest - 1)
            end
          in
          let child_level =
            match typ with
            | Cell_type.Merkle_proof | Cell_type.Merkle_update -> level_i + 1
            | _ -> level_i
          in
          if child_level > 3 then raise (Fail Invalid_hash_layout);
          let d = ref 0 in
          Array.iter (fun c -> d := max !d (depth_at c child_level)) refs;
          if Array.length refs > 0 then incr d;
          if !d > max_depth then raise (Fail (Depth_overflow !d));
          depths.(dest) <- !d;
          hashes.(dest) <-
            sha256
              (repr ~original_bits:bits ~cur_bits ~refs ~level:level_i
                 ~mask:(Level_mask.value (Level_mask.apply m level_i))
                 ~typ);
          incr hash_i
        end
    done;
    (* Expand into a dense 4-entry table indexed by level. *)
    let rh = Array.make 4 "" and rd = Array.make 4 0 in
    let top = Level_mask.hash_index m in
    for i = 0 to 3 do
      let hi = Level_mask.hash_index (Level_mask.apply m i) in
      match pruned with
      | Some entries when hi <> top ->
          if hi >= Array.length entries then raise (Fail Invalid_hash_layout);
          rh.(i) <- entries.(hi).p_hash;
          rd.(i) <- entries.(hi).p_depth
      | Some _ ->
          rh.(i) <- hashes.(0);
          rd.(i) <- depths.(0)
      | None ->
          if hi >= hash_count then raise (Fail Invalid_hash_layout);
          rh.(i) <- hashes.(hi);
          rd.(i) <- depths.(hi)
    done;
    Ok (m, rh, rd)
  with Fail e -> Error e

(* --- construction --------------------------------------------------------- *)

let make ~exotic bits refs =
  let nbits = Bits.length bits and nrefs = List.length refs in
  if nbits > max_bits then Error (Too_many_bits nbits)
  else if nrefs > max_refs then Error (Too_many_refs nrefs)
  else
    let* typ =
      if not exotic then Ok Cell_type.Ordinary
      else if nbits < 8 then Error (Exotic_too_short nbits)
      else
        let tag = u8 bits 0 in
        match Cell_type.of_tag tag with
        | Some t -> Ok t
        | None -> Error (Exotic_unknown_type tag)
    in
    let refs = Array.of_list refs in
    let* mask, hashes, depths = wonder typ bits refs in
    Ok { typ; bits; refs; mask; hashes; depths }

let empty =
  match make ~exotic:false Bits.empty [] with
  | Ok c -> c
  | Error _ -> assert false (* no bits, no refs: cannot fail *)

(* --- access --------------------------------------------------------------- *)

let bits t = t.bits
let refs t = Array.to_list t.refs
let ref_count t = Array.length t.refs
let nth_ref t i = if i < 0 || i >= Array.length t.refs then None else Some t.refs.(i)
let cell_type t = t.typ
let is_exotic t = Cell_type.is_exotic t.typ
let mask t = t.mask
let level t = Level_mask.level t.mask

let check_level name level =
  if level < 0 || level > 3 then
    invalid_arg (Printf.sprintf "Cell.%s: level must be 0..3, got %d" name level)

let hash ?(level = 0) t =
  check_level "hash" level;
  t.hashes.(level)

let depth ?(level = 0) t =
  check_level "depth" level;
  t.depths.(level)

(* Identity is the level-3 hash, not the level-0 representation hash: a pruned
   branch deliberately reports the replaced subtree's hash at level 0, so
   deduplicating on that would merge two genuinely different cells and corrupt
   the tree. *)
let identity t = t.hashes.(3)
let equal a b = String.equal a.hashes.(3) b.hashes.(3)
let compare a b = String.compare a.hashes.(3) b.hashes.(3)

let pp ppf t =
  let hex s = String.concat "" (List.init (String.length s) (fun i -> Printf.sprintf "%02x" (Char.code s.[i]))) in
  Format.fprintf ppf "@[<v 2>%a x{%a} (%d bits, %d refs) hash=%s@]" Cell_type.pp t.typ Bits.pp t.bits
    (Bits.length t.bits) (Array.length t.refs) (hex t.hashes.(0))
