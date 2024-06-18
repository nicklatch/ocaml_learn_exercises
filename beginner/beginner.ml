let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t

let rec last_two = function
  | [] | [ _ ] -> None
  | [ h; t ] -> Some (h, t)
  | _ :: t -> last_two t
