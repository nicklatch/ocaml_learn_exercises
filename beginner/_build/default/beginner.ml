let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t

let last_two = function
  | [] | [ _ ] -> None
  | [ h; t ] -> Some (h, t)
  | _ -> failwith "Testing"
(*  | _ :: t -> last_two t *)
