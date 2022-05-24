
let read file = 
  let stream = open_in file in
  let rec aux acc = 
    match input_line stream with
    | s -> aux (s::acc)
    | exception End_of_file -> acc
  in List.rev (aux [])

let det_bit l = 
    let rec aux l z o = 
        match l with
        | h::t -> (
            if String.length h = 0 then None else
            (
                if String.get h 0 = '0' then (aux t (z + 1) o) else (aux t z (o + 1))
            )
        )
        | [] -> (
            if 2 * z >= z + o then Some "0" else Some "1"
        )
    in aux l 0 0

let del_first_char l = List.map (fun s -> String.sub s 1 (String.length s - 1)) l

let gamma_rate_string l = 
  let rec aux l acc =
    let d = det_bit l in 
      match d with
      | Some s -> aux (del_first_char l) (acc ^ s)
      | None -> acc
  in aux l ""

let epsilon_rate_string gr = 
  let rec aux s acc = 
    match s with
    | "" -> acc
    | s -> aux (String.sub s 1 ((String.length s) - 1)) (acc ^ (if String.get s 0 = '0' then "1" else "0"))
  in aux gr ""


let main() = 
  let file = read "data-d3.txt" in
  let gr_s = gamma_rate_string file in
  let gr = int_of_string ("0b" ^ gr_s) in
  let er = int_of_string ("0b" ^ epsilon_rate_string gr_s) in 
  print_int (gr * er)