let tokenize code =
  let rec explode string =
    if String.length string = 0 then []
    else [String.sub string 0 1] @
         explode (String.sub string 1 ((String.length string) - 1))
  in
  let specialChars =
    [
      "["; "]"; "-"; "+"; "*"; "/"; "("; ")"
    ]
  in
  let nonSpecialHelper = function
    | "" -> []
    | str -> [str]
  in
  let rec tokenizeHelper = function
    | [],currentItem, tokens ->
      tokens @ (nonSpecialHelper currentItem)
    | " "::lst, currentItem, tokens ->
      tokenizeHelper(
        lst,"",
        tokens @ nonSpecialHelper currentItem)
    | item::lst, currentItem, tokens ->
      if List.mem item specialChars then
        tokenizeHelper(
          lst, "",
          tokens @ nonSpecialHelper currentItem @ [item])
      else
        tokenizeHelper(lst, currentItem ^ item,tokens)
  in
  tokenizeHelper(explode code, "", [])

type ast =
  | Imm of int  (* immediate value *)
  | Arg of int  (* reference to n-th argument *)
  | Add of (ast * ast) (* add first to second *)
  | Sub of (ast * ast) (* subtract second from first *)
  | Mul of (ast * ast) (* multiply first by second *)
  | Div of (ast * ast) (* divide first by second *)

exception CompilerError of string

module type COMPILER =
sig
  val pass1: string -> ast
  val pass2: ast -> ast
  val codeGen: ast -> string list
  val compile: string -> string list 
end;;


module Compiler : COMPILER =
struct
  let pass1 code = 
    raise (CompilerError "missing implementation")

  let pass2 ast =
    raise (CompilerError "missing implementation")  

  let codeGen = 
    raise (CompilerError "missing implementation")

  let compile code =
    codeGen(pass2(pass1 code))
end;;

let rec simualte : string list * int list -> int =
  let stack = Stack.create () in
  let r0 = ref 0 in
  let r1 = ref 0 in
  function
  | ([],argumets) -> !r0
  | ("SU"::lst,argumets) ->
    r0 := !r0 - !r1;
    simualte(lst,argumets)
  | ("DI"::lst,argumets) ->
    r0 := !r0 / !r1;
    simualte(lst,argumets)
  | ("MU"::lst,argumets) ->
    r0 := !r0 * !r1;
    simualte(lst,argumets)
  | ("AD"::lst,argumets) ->
    r0 := !r0 + !r1;
    simualte(lst,argumets)
  | ("PU"::lst,argumets) ->
    Stack.push !r0 stack;
    simualte(lst,argumets)
  | ("PO"::lst,argumets) ->
    r0 := (Stack.pop stack);
    simualte(lst,argumets)
  | ("SW"::lst,argumets) ->
    let tmp = !r0 in
    r0 := !r1;
    r1 := tmp;
    simualte(lst,argumets)
  | (op::lst,argumets) ->
    let op_code = String.sub op 0 2 in
    let value =
      int_of_string
        (String.sub op 3 ((String.length op) - 3))
    in
    match op_code with
    | "IM" ->
      r0 := value;
      simualte(lst,argumets)
    | "AR" ->
      r0 := List.nth argumets value;
      simualte(lst,argumets)
    | _ -> raise (CompilerError "bad assembly")