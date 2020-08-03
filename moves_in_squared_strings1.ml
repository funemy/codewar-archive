let hor_mirror (s: string): string = 
  let slist = String.split_on_char '\n' s in
  let len = List.length slist in
  let rec aux res = function
    | 0 -> (List.nth slist 0) :: []
    | n -> (List.nth slist n) :: (aux res (n-1))
  in String.concat "\n" (aux [] (len-1))
;;

let s = "abcd\nefgh\nijkl\nmnop";;
let test1 = hor_mirror s;;

let rev s = 
  let rec aux n res = if n==0 then 
      res ^ String.make 1 s.[0]
    else 
      aux (n-1) (res ^ String.make 1 s.[n]) 
  in
  aux ((String.length s) - 1) ""
;;

let vert_mirror (s: string): string =
  let slist = String.split_on_char '\n' s in
  let rec rev_list = function
    | [] -> []
    | s :: slist -> (rev s) :: rev_list slist
  in String.concat "\n" (rev_list slist)
;;

let test2 = vert_mirror s;;

let oper f s = f s;;

let test3 = oper hor_mirror s;;
let test4 = oper vert_mirror s;;