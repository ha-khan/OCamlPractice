(*
    sqsum : int list -> int

    Function that uses List.fold_left to square
    the integers in an integer list, and storing
    their values in an accumulator, that will output
    and integer lists squared sum. Only uses List.fold_left 
    to traverse the integer list. 
*)
let sqsum xs = 
  let f a x = a + x*x in
  let base = 0 in
    List.fold_left f base xs






(*
    pipe : ('a -> 'a) list -> 'a -> 'a 

    Function that takes a list of functions that
    makes a composition of them. The base case is
    the identity function since if the list of functions
    is empty, then it will just return the integer that 
    would have been the starting input. Originally my problem
    was that I didn't have a function to account for the integer
    out in front and I kept getting the error message that had a
    mis match error. 

*)
let pipe fs = 
  let f a x = fun c -> x(a(c)) in
  let base = fun b -> b in
    List.fold_left f base fs




(*
    sepConcat : string -> string list -> string

    Function that takes a delimitter, and a string list
    and separates the values in the stringlist by the 
    delimitter. Works by getting the head of the string list
    and setting it as the base, and the tail of the string list
    as the the list that List.fold_left will be iterating over and
    applying the function f upon its elements. The function f is a
    curried function that concatenates the head element, delimmiter,
    and recursivley calls sepConcat.

*)
let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = h^sep^sepConcat sep t in
      let base = h in
      let l = t in
        List.fold_left f base l






(*
    stringOfList : ('a -> string) -> 'a list -> string

    Function that works simply by using sepConcate to separate 
    each value of the list l that has the function f applied to
    it. This function uses List.map to iterate over the elements,
    since there was no need to accumulate the values that the 
    function f worked upon. ^ was used to concatenate the [] operators
    to output the string. 
*)
let stringOfList f l = "["^(sepConcat "; " (List.map f l))^"]"





(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)




(*
    clone : 'a -> int -> 'a list

    Function that is relatively simple in that it takes a value
    and an integer, and makes a list that contains the same element
    the number of time the integer was specified at. Works recursivley
    by using :: which bottoms out when the int = 0, which will return 
    [].
*)
let rec clone x n = if n <= 0 then [] else x::clone x (n-1)








(*
    padZero : int list -> int list -> int list * int list

    Function that makes two lists the same length by making the one with 
    the smaller length have 0's appended to the front of it by using the 
    clone function and a difference in their length. Base case is if they 
    are equal length, which returns the tuple of them, else the if statement 
    will decide which is the smaller and recursivley call padZero with the 
    new list. 
*)
let rec padZero l1 l2 = if List.length l1 = List.length l2 then (l1, l2)
  else if (List.length l1) < (List.length l2) then ((clone 0 ((List.length l2)-(List.length l1)))@l1, l2)
    else (l1, (clone 0 ((List.length l1)-(List.length l2)))@l2)









(*
    removeZero : int list -> int list

    Function that is a simple recursive function that
    removes the front trailing 0's by checking to see
    if the head is a 0. If not then it will end by outputting
    the list. Else the tail will be recursively called, and the
    head of it checked, and so on. 
*)
let rec removeZero l = match l with
  | h::t -> if h != 0 then h::t else removeZero t
  | [] -> []







(*
    tupleAdd : int * int -> int

    Simple function that adds two elements in a tuple and returns that 
    value. 
*)
let tupleAdd (x,y) = x+y






(*
    getTupleFirst : 'a * 'b -> 'a 

    Function that just gets the first element of a tuple. 
*)
let getTupleFirst (x,y) = x




(*
    getTupleSecond : 'a * 'b -> 'b

    Function that gets the second element of a tuple.  
*)
let getTupleSecond (x,y) = y




(*
    isUnderTwo : int -> bool 

    Function that returns whether an integer has fewer
    than two digits. 
*)
let isUnderTwo x = if x <= 9 then true else false





(*
    isLastElement : 'a list -> 'b list -> bool

    Function that is strictly used for my implementation of
    bigAdd, since I ran into the problem where if there was 
    a number that was added, and the last addition yielded
    a carry over value, then it wouldn't save. So to fix it
    I had to create this function that would tell me if I was
    about to add the last element in the list. 
*)
let isLastElement x y = if List.length x = (List.length y - 1) then true else false





(*
    getNextAdd : int -> int

    Function that recursivley works by dividing an integer until it
    is a single digit, in which case it will return that digit, It is
    used to see how much I should carry over to the next addition. 
*)
let rec getNextAdd x = if x < 9 then x else (getNextAdd (x/10))  





(*
    bigAdd : int list -> int list -> int list

    Checks if the sum of the carry and tuple is under two digits, if
    it is then another tuple is sent where the carry is 0, and the list
    that contains the sum has the integer added to it. Then it will
    check if the sum is the last digits of the integer lists, if it is
    then it has to specially take care of the values that will come from 
    the addition and append that sum to the list. Lastly,
    If the sum of the tuple and the carry is greater than two then 
    it will work much like if the sum was under two digits, only this time 
    using mod 10 to get the right most integer. 
    getNextAdd is used extensivly to determine what the carry will be.  
      Base argumentis a tuple which contains 0, meaning no carry, and [] which is just an 
    empty list since nothing has been added. Function works mainly by using
    List.fold_left to iterate like the previous functions and saves the carry
    result and the list containing the mod 10 of the integers that have been 
    added thus far. The list that will be iterater over is the reverse combine 
    of l1, l2. This makes a tuple that groups the elements that are to be added. 
    it is reversed because List.fold_left works from left to right, and adding
    is usually done from right to left. 
*)
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = if isUnderTwo ((tupleAdd x) + (getTupleFirst a))
                then (0,((tupleAdd x) + (getTupleFirst a))::(getTupleSecond a))
                else 
                  if isLastElement (getTupleSecond a) l2 
                  then (0, ((getNextAdd ((tupleAdd x) + (getTupleFirst a)))::((tupleAdd x) + (getTupleFirst a))mod 10::(getTupleSecond a)))
                  else (getNextAdd ((tupleAdd x) + (getTupleFirst a)), ((tupleAdd x) + (getTupleFirst a))mod 10::(getTupleSecond a)) 
              in
    let base = (0,[]) in
    let args = List.rev(List.combine l1 l2)  in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))






(*
    mulByDigit : int -> int list -> int list

    Function that is simple, it works by adding each element of a cloned version
    of the integer list by the same value, the amount of times that need to be multiplied by. 
*)
let rec mulByDigit i l = if i = 0 then [] else List.fold_left bigAdd l (clone l (i-1))






(*
    bigMul : int list -> int list -> int list

    Function that multiplies two integer lists, works somewhat similarly to bigAdd, though
    it is much simpler. base/args are similar to bigAdd, except there didn't need to be any combine
    operation. bigMul relies heavily on bigAdd, and mulBydigit, and clone. By multByDigit each
    value in l2, to the integer list l1, it then appends the respective 0's to the multiplication
    to give the appropriate multiplication place. It then adds to the previous list/sum. 
*)
let bigMul l1 l2 = 
  let f a x = ( (getTupleFirst a) + 1, bigAdd ((mulByDigit (x) (l1))@(clone 0 (getTupleFirst a))) (getTupleSecond a)) in
  let base = (0,[]) in
  let args = List.rev(l2) in
  let (_, res) = List.fold_left f base args in
    res
