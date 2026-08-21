module C = Mirage_crypto.AES.CTR

(* [block] holds the unconsumed tail of the last keystream chunk, so a call
   that ends mid-block leaves the remainder for the next one. *)
type t = { key : C.key; ctr : C.ctr; block : string; used : int; offset : int }

let chunk = 4096

let create ~key ~iv =
  if String.length key <> 32 then invalid_arg "Ctr.create: key must be 32 bytes";
  if String.length iv <> 16 then invalid_arg "Ctr.create: iv must be 16 bytes";
  { key = C.of_secret key; ctr = C.ctr_of_octets iv; block = ""; used = 0; offset = 0 }

let xor t s =
  let n = String.length s in
  let out = Bytes.create n in
  let rec go t i =
    if i >= n then t
    else
      let avail = String.length t.block - t.used in
      if avail = 0 then begin
        (* Refill on whole blocks only; the counter may then be advanced by a
           block count, which is the one arithmetic the cipher guarantees. *)
        let want = min chunk (max C.block_size (((n - i) + C.block_size - 1) / C.block_size * C.block_size)) in
        let block = C.stream ~key:t.key ~ctr:t.ctr want in
        go { t with ctr = C.add_ctr t.ctr (Int64.of_int (want / C.block_size)); block; used = 0 } i
      end
      else begin
        let take = min (n - i) avail in
        for k = 0 to take - 1 do
          Bytes.unsafe_set out (i + k)
            (Char.unsafe_chr (Char.code (String.unsafe_get s (i + k)) lxor Char.code (String.unsafe_get t.block (t.used + k))))
        done;
        go { t with used = t.used + take; offset = t.offset + take } (i + take)
      end
  in
  let t = go t 0 in
  (t, Bytes.unsafe_to_string out)

let offset t = t.offset
