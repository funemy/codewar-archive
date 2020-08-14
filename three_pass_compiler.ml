let print_list l = let open Printf in List.iter (printf "%s ") l; print_newline();;

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
    let token = tokenize code in
    (* parse argument list *)
    let rec parse_args tok res = match tok with 
      | "]" :: t -> t, List.rev res
      | "[" :: t -> parse_args t res
      | h :: t -> parse_args t (h :: res)
      | _ -> raise (CompilerError "parse arg list error") in
    let prog, args = parse_args token [] in
    (* find argument index *)
    let arg_no s =
      let rec aux s i = function
        | [] -> raise (CompilerError ("PARSING ERROR: unbound argument " ^ s))
        | h :: t -> if s = h then i else aux s (i+1) t
      in aux s 0 args in
    (* process operators *)
    let process_op factor op = 
      (* we only have bi-operators *)
      let f1, f2, t = match factor with
        | f1 :: f2 :: t -> f1, f2, t
        | _ -> raise (CompilerError ("PARSING ERROR: not enough operand for " ^ op)) in 
      let tem = match op with
        | "+" -> Add (f2, f1) 
        | "-" -> Sub (f2, f1) 
        | "*" -> Mul (f2, f1) 
        | "/" -> Div (f2, f1)
        | _ -> raise (CompilerError ("PARSING ERROR: unsupported operator " ^ op)) in
      tem :: t in
    (* process the program inside a pair of parentheses *)
    let rec process_paren factor ops = match ops with
      | "(" :: t -> factor, t
      | h :: t -> process_paren (process_op factor h) t
      | _ -> raise (CompilerError "PARSING ERROR: parentheses mismatch") in
    (* process all * and / operators on the top of the stack *)
    let rec process_term factor ops = match ops with 
      | "*" :: t -> process_term (process_op factor "*") t
      | "/" :: t -> process_term (process_op factor "/") t
      | ops -> factor, ops in
    (* finish traversing the token list, genereate the final ast *)
    let rec process_all factor ops = 
      match ops with
      | h :: t -> process_all (process_op factor h) t
      | [] -> (match factor with
          | [f] -> f
          | _ -> raise (CompilerError "PARSING ERROR: incomplete program")) in
    let rec parse p factor ops = match p with
      | "(" :: t -> parse t factor ("(" :: ops)
      | ")" :: t -> let f, o = process_paren factor ops in parse t f o
      | "+" :: t -> let f, o = process_term factor ops in parse t f ("+" :: o)
      | "-" :: t -> let f, o = process_term factor ops in parse t f ("-" :: o)
      | "*" :: t -> parse t factor ("*" :: ops)
      | "/" :: t -> parse t factor ("/" :: ops)
      | h :: t -> (match int_of_string_opt h with
          | None -> parse t ((Arg (arg_no h)) :: factor) ops
          | Some i -> parse t ((Imm i) :: factor) ops)
      | [] -> process_all factor ops
    in parse prog [] []


  let rec pass2 ast = 
    match ast with
    | Add a -> eval ( + ) (fun x -> Add x) a
    | Sub a -> eval ( - ) (fun x -> Sub x) a
    | Mul a -> eval ( * ) (fun x -> Mul x) a
    | Div a -> eval ( / ) (fun x -> Div x) a
    | ast -> ast
  (* to save some space  *)
  and eval f ctor (a1, a2) = match (pass2 a1), (pass2 a2) with
    | Imm i1, Imm i2 -> Imm (f i1 i2)
    | a1, a2 -> ctor (a1, a2)


  (* This is incomplete *)
  let codeGen ast = 
    let rec aux ast res = match ast with
      | Imm i -> "SW" :: ("IM " ^ string_of_int i) :: res
      | Arg n -> ("AR " ^ string_of_int n) :: res
      | Add (a1, a2) -> "AD" :: (aux a1 (aux a2 res))
      | _ -> raise (CompilerError "unimplemented")
    in List.rev (aux ast [])

  let compile code =
    codeGen(pass2(pass1 code))
end;;

let test_pass1 = Compiler.pass1 "[ x y ] 2 / y + x"
let test_pass1 = Compiler.pass1 "[ x y ] ( x + y ) / 2"
let test_pass1 = Compiler.pass1 "[ x ] x + 2*5"


(* let test_pass2 = Compiler.pass2 test_pass1
   let test_pass3 = Compiler.codeGen test_pass2 *)

(* let test_pass1 = Compiler.pass1 "[ x y ] ( x + y ) / 2" *)

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