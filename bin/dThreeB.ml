

let read file = 
  let stream = open_in file in
  let rec aux acc = 
    match input_line stream with
    | s -> aux (s::acc)
    | exception End_of_file -> acc
  in List.rev (aux [])

let o_bit_criteria l n =
  let rec aux l z o =
    match l with
    | h::t -> (if String.length h = n then None 
              else (
                if String.get h n = '0' then aux t (z + 1) o else aux t z (o + 1)
              ))
    | [] -> (if 2 * o >= z + o then Some "1" else Some "0")
  in aux l 0 0

let co_bit_criteria l n = 
  let ob = o_bit_criteria l n in
    match ob with 
    | None -> None
    | Some s -> Some (string_of_int  (((int_of_string s) + 1) mod 2))

let print_string_opt s = 
  match s with 
  | None -> print_string "None"
  | Some x -> print_string ("Some " ^ x)


let rec loop_o l n =
  match l with 
  | [] -> None
  | x::[] -> Some x
  | _ -> (
    let bit = o_bit_criteria l n in
    match bit with
    | None -> loop_o [] (n+1)
    | Some(b) -> loop_o (List.filter (fun x -> String.get x n = String.get b 0) l) (n+1)
  )


let rec loop_co l n =
    match l with 
  | [] -> None
  | x::[] -> Some x
  | _ -> (
    let bit = co_bit_criteria l n in
    match bit with
    | None -> loop_co [] (n+1)
    | Some(b) -> loop_co (List.filter (fun x -> String.get x n = String.get b 0) l) (n+1)
  )

let int_opt_of_string_opt = function
  | None -> None 
  | Some s -> Some(int_of_string("0b" ^ s))

let print_int_opt = function
  | None -> print_string "None"
  | Some x -> print_string ("Some " ^ string_of_int x)

let mult_opt x y = 
  match x with
  | None -> None
  | Some a -> (match y with 
    | None -> None
    | Some b -> Some (a * b)
  )

let main() = 
  let l = read "data-d3.txt" in 
  let o = int_opt_of_string_opt (loop_o l 0 ) in
  let co = int_opt_of_string_opt (loop_co l 0 ) in
  print_int_opt (o);
  print_newline();
  print_int_opt (co);
  print_newline();
  print_string "Result: " ;
  print_int_opt (mult_opt o co);
  print_endline ""