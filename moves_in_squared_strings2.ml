let rev s = 
  s |> Str.split (Str.regexp "") |> List.rev |> String.concat ""
;;

let test1 = rev "abcde";;

let rot(s: string): string = 
  s |> String.split_on_char '\n' |> List.map rev |> List.rev |> String.concat "\n"
;;

let test2 = rot "ponm\nlkji\nhgfe\ndcba";;

let selfie_and_rot (s: string): string =
  let selfie = s 
               |> String.split_on_char '\n' 
               |> List.map (fun x -> x ^ String.make (String.length x) '.') 
               |> String.concat "\n" in
  let rot = s 
            |> String.split_on_char '\n' 
            |> List.map (fun x -> String.make (String.length x) '.' ^ rev x) 
            |> List.rev 
            |> String.concat "\n" in
  selfie ^ "\n" ^ rot
;;

let test3 = selfie_and_rot "abcd\nefgh\nijkl\nmnop";;

let oper f s = f s;;