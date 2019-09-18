let rec reverse_list l =
  let rec aux acc = function
    (* we defined anonymous support function with argument `acc`.
       There is two similar words for doing so: first one is `fun` and
       second is `function`. Latter one accepts only one argument but
       allows to use pattern matching directly without need to type
       `match arg with ...`. `fun` on the other hand does not allow
       pattern matching but supports passing multiple arguments. (from
       SO) *)
    | [] -> acc (* if list is empty return it *)
    | hd :: tl -> aux (hd :: acc) tl
  in (* we defined `aux` and now using it in expression *)
  aux [] l;;

let rec print_list l =
  match l with
  | [] -> ()
  | hd :: tl -> print_int hd ; print_string " " ; print_list tl



let l = [1; 2; 3];;

print_list (reverse_list l)
