

let rec fibonacci = function n ->
  match n with 
  | 0 -> 0
  | 1 -> 1
  | _ -> fibonacci (n - 1) + fibonacci (n - 2);;


let main = function () ->
  print_newline (print_int (fibonacci 40));;

main ()
