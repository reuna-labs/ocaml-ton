(* Turns the BIP-39 English wordlist into an OCaml array so the library needs
   no data files at runtime -- a unikernel has no filesystem to read them
   from. *)
let () =
  let ic = open_in Sys.argv.(1) in
  let words = ref [] in
  (try
     while true do
       let w = String.trim (input_line ic) in
       if w <> "" then words := w :: !words
     done
   with End_of_file -> close_in ic);
  let words = Array.of_list (List.rev !words) in
  print_string "(* Generated from english.txt by gen/gen_wordlist.ml. Do not edit. *)\n\n";
  Printf.printf "let count = %d\n\nlet words = [|\n" (Array.length words);
  Array.iteri (fun i w -> Printf.printf "  %S;%s" w (if i mod 4 = 3 then "\n" else "")) words;
  print_string "\n|]\n"
