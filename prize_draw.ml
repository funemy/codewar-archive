(* compute the point of a name *)
let name_pt name =
  let name = String.lowercase_ascii name in
  let l = String.length name in
  let char_pt c = (Char.code c) - 96 in
  let rec str_pt s idx pt = match idx with
    | 0 -> (char_pt s.[0]) + pt
    | idx -> (char_pt s.[idx]) + str_pt s (idx - 1) pt
  in l + str_pt name (l - 1) 0
;;

let test1 = name_pt "PauL";;

(* weighted name point *)
let wname_pt name w = w * name_pt name;;

let test2 = wname_pt "PauL" 2;;

let compare_tuple t1 t2 = match t1, t2 with
  | (p1, n1), (p2, n2) -> if p1 = p2 then String.compare n2 n1 else (p1 - p2)
;;

let rank st we n =
  let name_list = String.split_on_char ',' st  in
  let num = List.length name_list in
  let weights = Array.to_list we in
  let rec aux nl ws res = match nl, ws with
    | [], [] -> []
    | n :: ns, w :: ws -> ((wname_pt n w), n) :: aux ns ws res
    | _, _ -> res
  in
  match name_list with
    | [""] -> "No participants"
    | _ -> if num < n then "Not enough participants"
    else let res = aux name_list weights [] in
      let sorted_res = List.sort compare_tuple res in
      (* List.iter (fun x -> print_int (fst x); print_char ' '; print_string (snd x); print_char '\n') sorted_res; *)
      snd (List.nth sorted_res (num - n))
;;

let test4 = rank "Addison,Jayden,Sofia,Michael,Andrew,Lily,Benjamin" [|4; 2; 1; 4; 3; 1; 2|] 4;;
let test5 = rank "Sophia,Robert,Abigail,Grace,Lagon" [|1;2;2;6;4|] 3;;
let test6= rank "Elijah,Chloe,Elizabeth,Matthew,Natalie,Jayden" [|1;3;5;5;3;6|] 2;;
let test6= rank "" [|1;3;5;5;3;6|] 2;;

