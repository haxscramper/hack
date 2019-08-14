let head l =
  match l with
  | [] -> None
  | hd :: tl -> Some hd

let tmp = [1; 2; 3];;

head tmp
