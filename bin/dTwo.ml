
exception Unknown_operation

let forward1 (c: int * int) n = let (x, y) = c in (x+n, y)

let down1 (c: int * int) n = let (x, y) = c in (x, y+n)

let up1 (c: int * int) n = let (x, y) = c in (x, y-n)

let read file = 
  let stream = open_in file in
  let rec aux acc = 
    match input_line stream with
    | s -> aux (s::acc)
    | exception End_of_file -> acc
  in List.rev (aux [])

let rec loop1 l c = 
  match l with
  | [] -> c
  | h::t -> (
    match String.split_on_char ' ' h with 
    | "forward"::s::_ -> loop1 t (forward1 c (int_of_string s))
    | "down"::s::_ -> loop1 t (down1 c (int_of_string s))
    | "up"::s::_ -> loop1 t (up1 c (int_of_string s))
    | _-> raise Unknown_operation
  )


let forward2 (c: int * int * int) n = let (x, y, aim) = c in (x + n, y + (aim * n), aim)

let down2 (c: int * int * int) n = let (x, y, aim) = c in (x, y, aim + n)

let up2 (c: int * int * int) n = let (x, y, aim) = c in (x, y, aim - n)

let rec loop2 l c = 
  match l with
  | [] -> c
  | h::t -> (
    match String.split_on_char ' ' h with 
    | "forward"::s::_ -> loop2 t (forward2 c (int_of_string s))
    | "down"::s::_ -> loop2 t (down2 c (int_of_string s))
    | "up"::s::_ -> loop2 t (up2 c (int_of_string s))
    | _-> raise Unknown_operation
  )

let main() = let r = read "data-d2.txt" in 
  let (x1, y1) = loop1 r (0, 0) in print_int (x1 * y1);
  print_newline();
  let (x2, y2, _) = loop2 r (0, 0, 0) in print_int (x2 * y2)
  
