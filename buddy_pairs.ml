let divisor n = 
  let rec aux s res = 
    if s <= n/2 then
      if (n mod s) = 0 then aux (s+1) (s :: res) else aux (s + 1) res
    else res in
  aux 1 []
;;

let test1 = divisor 100;;
let test2 = divisor 48;;
let test3 = divisor 75;;

let rec sum = function
  | [] -> 0
  | x :: xs -> x + sum xs
;;

let test4 = sum test2
let test5 = sum test3

let buddy (start: int) (nd: int): string =
  let rec aux n = if n <= nd then 
      let ds = divisor n in
      let potential = (sum ds) - 1 in
      let ds' = sum (divisor potential) - 1 in
      if potential > start && ds' = n then string_of_int n ^ " " ^ string_of_int potential
      else aux (n+1)
    else "Nothing" 
  in aux start
;;

let test6 = buddy 10 50;;
let test7 = buddy 48 50;;
let test8 = buddy 2177 2300;;