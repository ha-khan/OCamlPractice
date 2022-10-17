open Owl
open Owl.Arr

let x = Arr.sequential [|1;3;5|];;

let b = Arr.max' x;;

let x = Arr.elt_to_float b;;

print_newline (print_float b);;

(* ocamlfind ocamlc -package owl -linkpkg main.ml -o main *)
