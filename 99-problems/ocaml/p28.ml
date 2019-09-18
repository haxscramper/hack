let rec insertion_sort = function
  | [] -> []
  | hd :: tl -> insert hd (insertion_sort tl)
and insert el = function
  | [] -> [el]
  | hd :: tl ->
    if el < hd then el :: hd :: tl
    else hd :: insert el tl;;

let rec filter pred = function
  | [] -> []
  | hd :: tl ->
    if pred hd then hd :: filter pred tl
    else filter pred tl

let rec quicksort = function
  | [] -> []
  | hd :: tl ->
    quicksort (filter (fun x -> x < hd) tl) @ [hd] @
    quicksort (filter (fun x -> x > hd) tl)


let rec print_list l =
  match l with
  | [] -> print_string "\n"
  | hd :: tl -> print_int hd ; print_string " " ; print_list tl

let lst = [1; 22; 0; -1; -90];;

print_list (insertion_sort lst);;
print_list (quicksort lst);;
