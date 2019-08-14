let rec pre_last l =
  match l with
  | [] -> None
  | hd :: [] -> None
  | hd :: it :: [] -> Some hd
  | hd :: tl -> pre_last tl



let l = [1; 2; 3];;

pre_last l
