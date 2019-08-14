let rec tail l =
  match l with
  | [] -> None
  | hd :: [] -> Some hd
  | hd :: tl -> tail tl


let l = [1; 2; 3];;

tail l
