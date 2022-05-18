exception NotEnoughData

let read file = 
  let stream = open_in file in
  let rec aux acc = 
    match input_line stream with
    | s -> aux ((int_of_string s)::acc)
    | exception End_of_file -> acc
  in List.rev (aux [])

let count l = 
  let rec aux l acc = 
    match l with
    | c::s::t -> ( if c < s then aux (s::t) (acc+1) else aux (s::t) acc )
    | _ -> acc
  in aux l 0

let sum3 l =
match l with
| c0::c1::c2::c3::_ -> (c0 + c1 + c2, c1 + c2 + c3)
| _ -> raise NotEnoughData

let count2 l = 
  let rec aux l acc = 
    try
      let (a, b) = sum3 l in if a < b then aux (List.tl l) (acc+1) else aux (List.tl l) acc
    with NotEnoughData -> acc
  in aux l 0

let main() = 
  let re = read "data-d1.txt" in
  print_int (count re);
  print_newline();
  print_int (count2 re);
  print_endline("")
