let rec list_size l =
  match l with
  | [] -> 0
  | hd :: [] -> 1
  | hd :: tl -> 1 + (list_size tl)

let l = [1; 2; 3];;

print_int (list_size l)
