exception Exception of string

let begin_ f = f [];;

let push stack lit next = next (lit :: stack);;

let add stack next = match stack with
  | [] -> raise (Exception "adding empty stack")
  | [x] -> raise (Exception "adding single element stack")
  | h1 :: h2 :: t -> next (h1 + h2 :: t)
;;

let end_ stack = match stack with
  | [] -> raise (Exception "invalid program")
  | t :: h -> t
;;

let test1 = begin_ push 5 end_
let test2 = begin_ push 5 push 6 add end_
let test3 = begin_ push 1 push 1 add push 1 end_
let test4 = begin_ push 1 push 1 add push 2 add end_
let test5 = begin_ push 1 push 1 push 1 push 1 push 1 push 1 push 1 push 1 push 1
    add add add add add add add add end_