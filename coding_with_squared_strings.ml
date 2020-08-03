let find_align l =  
  let rec aux n = if n*n >= l then n else aux (n+1)
  in aux 1
;;

let rect_str n s =
  let rec aux res = function
    | 0 -> res
    | k -> aux (String.sub s (k*n - n) n :: res) (k-1)
  in String.concat "\n" (aux [] n)
;;

let test1 = rect_str 3 "123456789";;

let get_col n ss = 
  ss |> List.map (fun s -> String.make 1 s.[n]) |> String.concat ""
;;

let get_col_rev n ss = 
  ss |> List.map (fun s -> String.make 1 s.[n]) |> List.rev |> String.concat ""
;;

let rot_90_clock(s: string): string =
  let ss = String.split_on_char '\n' s in
  let rec aux res = function
    | 0 -> res
    | n -> aux (get_col_rev (n-1) ss :: res) (n-1)
  in match ss with
  | [] -> ""
  | h :: t -> String.concat "\n" (aux [] (String.length h))
;;

let rot_90_counter(s: string): string = 
  let ss = String.split_on_char '\n' s in
  let len = String.length (List.nth ss 0) in
  let rec aux res = function
    | 0 -> res
    | n -> aux (get_col (len - n) ss :: res) (n-1)
  in  String.concat "\n" (aux [] len)
;;

let code (s: string): string =
  let l = String.length s in
  let align = find_align l in
  let padded = s ^ String.make (align * align - l) '&'in
  let rect = rect_str align padded in
  rot_90_clock rect
;;

let test2 = code "Some say the world will end in fire, Some say in ice. From what I've tasted of desire";;

let decode (s: string): string =
  s 
  |> rot_90_counter 
  |> String.split_on_char '\n' 
  |> String.concat "" 
  |> String.split_on_char '&'
  |> List.hd
;;

let test3 = decode test2;;