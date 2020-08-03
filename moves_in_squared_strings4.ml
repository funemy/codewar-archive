let get_col n ss = 
  ss |> List.map (fun s -> Char.escaped s.[n]) |> String.concat ""
;;

let get_col_rev n ss = 
  ss |> List.map (fun s -> Char.escaped s.[n]) |> List.rev |> String.concat ""
;;

let op_on_str_list f ss = 
  let len = String.length (List.nth ss 0) in
  let rec aux res = function
    | 0 -> res
    | n -> aux (f (len - n) ss :: res) (n-1)
  in aux [] len
;;

let op_on_str f s = 
  let ss = String.split_on_char '\n' s in
  String.concat "\n" (op_on_str_list f ss)
;;

let diag_2_sym(s: string): string =
  op_on_str get_col_rev s
;;

let s = "abcd\nefgh\nijkl\nmnop";;
let test1 = diag_2_sym s;;

let rot_90_counter(s: string): string = 
  op_on_str get_col s
;;

let test2 = rot_90_counter s;;

let selfie_diag2_counterclock(s: string): string =
  let ss = String.split_on_char '\n' s in
  let help x y = x ^ "|" ^ y in 
  let diag2 = op_on_str_list get_col_rev ss in
  let cc = op_on_str_list get_col ss
  in cc |> List.map2 help (List.map2 help ss diag2) |> String.concat "\n"
;;

let test3 = selfie_diag2_counterclock s;;

let oper f s = f s;;