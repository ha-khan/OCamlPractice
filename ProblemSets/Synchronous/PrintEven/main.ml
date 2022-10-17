let list = [1;2;3;4;5;6;7;8;9;10];;

let rec print_even x = 
  match x with 
  | [] -> ()
  | h::t ->
    let is_even = h mod 2 == 0 in
      if is_even
        then begin
          print_newline (print_int h);
          print_even t
        end
      else
        print_even t;;

print_even list;;
