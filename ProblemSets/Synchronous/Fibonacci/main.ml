

let fibonacci = function n ->
  let rec helper n = 
    match n with 
    | 0 -> 0
    | 1 -> 1
    | _ -> helper(n - 1) + helper(n - 2)
  in
  helper n;;


let main = function () ->
  print_newline (print_int (fibonacci 4));;

main ();;
