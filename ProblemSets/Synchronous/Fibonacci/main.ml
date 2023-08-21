
let fibonacci n = 
  let memo = Hashtbl.create 1024 in 
    Hashtbl.add memo 0 0;
    Hashtbl.add memo 1 1;
    let rec helper n = 
      match Hashtbl.find_opt memo n with
        | None ->  let soln = (helper(n - 1) + (helper (n - 2))) in 
                     Hashtbl.add memo n soln; soln 
        | Some a -> a
  in helper n;;


let main = function () ->
  print_newline (print_int (fibonacci 60));;

main ();;
