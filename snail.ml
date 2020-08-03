let snail xs =
  let a = List.length xs in
  let xs = List.flatten xs in
  let rec cal_steps = function
    | 0 -> []
    | x -> if (x = a) then x :: cal_steps (x-1) else x :: x :: cal_steps (x-1) in
  (* number of iterations for each direction *)
  let steps = cal_steps a in
  (* directions *)
  let dirs = [|1; a; -1; -a|] in
  let rec do_step res prev interval = function
    | 0 -> prev, res
    | n -> let idx = prev + interval in
      do_step ((List.nth xs idx)  :: res) idx interval (n-1) in
  let rec aux res start i steps = match steps with
    | [] -> res
    | h :: t -> let (pos, res) = do_step res start dirs.(i) h in
      aux res pos ((i+1) mod 4) t
  in if xs = [] then [] else List.rev (aux [] ~-1 0 steps)
;;

let test1 = snail [[1;2;3];[4;5;6];[7;8;9]];;

(* a clever solution *)
let rec transpose = function
  | []     -> []
  | []::xs -> []
  | m      -> List.map List.hd m :: transpose (List.map List.tl m)
;;

let rec snail = function
  | [] -> []
  (* rev then transpose or transpose then rev both work *)
  (* as long as we are rotating clockwise *)
  | x::xs -> x @ (snail (transpose (List.map List.rev xs)))
;;