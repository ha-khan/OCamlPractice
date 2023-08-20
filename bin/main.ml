open Owl

let x = Arr.sequential [|1;3;5|];;

let b = Arr.max' x;;

let x = Arr.elt_to_float b  in 
  print_newline (print_float x);;

(* ocamlfind ocamlc -package owl -linkpkg main.ml -o main *)


let () = print_endline "Hello, World!"
