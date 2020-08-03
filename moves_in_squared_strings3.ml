let get_col n ss = 
  ss |> List.map (fun s -> Char.escaped s.[n]) |> String.concat ""
;;

let get_col_rev n ss = 
  ss |> List.map (fun s -> Char.escaped s.[n]) |> List.rev |> String.concat ""
;;

let diag_1_sym(s: string): string =
  let ss = String.split_on_char '\n' s in
  let rec aux res = function
    | 0 -> res
    | n -> aux (get_col (n-1) ss :: res) (n-1)
  in match ss with
  | [] -> ""
  | h :: t -> String.concat "\n" (aux [] (String.length h))
;;

let s = "abcd\nefgh\nijkl\nmnop";;
let test1 = diag_1_sym s;;

let rot_90_clock(s: string): string =
  let ss = String.split_on_char '\n' s in
  let rec aux res = function
    | 0 -> res
    | n -> aux (get_col_rev (n-1) ss :: res) (n-1)
  in match ss with
  | [] -> ""
  | h :: t -> String.concat "\n" (aux [] (String.length h))
;;

let test2 = rot_90_clock s;;

let selfie_and_diag1(s: string): string =
  let ss = String.split_on_char '\n' s in
  let rec aux res = function
    | 0 -> res
    | n -> aux (get_col (n-1) ss :: res) (n-1) in
  let diag1 = aux [] (String.length (List.nth ss 0)) in
  List.map2 (fun x y  -> x ^ "|" ^ y)  ss diag1 |> String.concat "\n"
;;

let test3 = selfie_and_diag1 s;;

let oper f s = f s;;