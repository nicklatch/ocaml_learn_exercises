let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t

let rec last_two = function
  | [] | [ _ ] -> None
  | [ h; t ] -> Some (h, t)
  | _ :: t -> last_two t

let rec nth_el el = function
  | [] -> None
  | h :: t -> if el = 0 then Some h else nth_el (el - 1) t

let length list =
  let rec aux acc = function
    | [] -> acc
    | _ :: t -> aux (acc + 1) t
  in
  aux 0 list

let rev list =
  let rec aux acc = function
    | [] -> []
    | h :: t -> aux (h :: acc) t
  in
  aux [] list
