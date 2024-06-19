let ( -- ) start stop =
  let rec aux f t l = if f > t then l else aux f (t - 1) (t :: l) in
  aux start stop []

let string_of_int_option = function
  | None -> "None"
  | Some v -> "Some " ^ string_of_int v

let string_of_int_int_option = function
  | None -> "None"
  | Some (v1, v2) -> "Some (" ^ string_of_int v1 ^ ", " ^ string_of_int v2 ^ ")"

let string_of_list string_of list =
  let rec aux acc = function
    | [] -> "[]"
    | [ x ] -> string_of x
    | h :: t -> aux (acc ^ " " ^ string_of h ^ ";") t
  in
  let contents = aux "" list in
  "[" ^ contents ^ "]"
