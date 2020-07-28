(* CREDIT: http://loup-vaillant.fr/tutorials/earley-parsing/semantic-actions *)

(** ************************** **)
(** Pervasive Helper functions **)
(** ************************** **)
let (|>) x f   = f x
let (|-) f g x = g (f x)
let id x       = x
let swap f x y = f y x
let const x y  = x
let (>>=) x f = match x with None -> None | Some r -> f r (* Maybe monad! *)
let some x = Some x

(** ***************************************** **)
(** Dynamic arrays                            **)
(**                                           **)
(** Damn Ocaml for not having them by default **)
(** ***************************************** **)
module type Dyn_array_sig =
  sig
    type 'a t
    val make : int  -> 'a -> 'a t
    val make_empty : unit -> 'a t
    val of_array : 'a array -> 'a t
    val get : 'a t -> int -> 'a
    val set : 'a t -> int -> 'a -> unit
    val (>:): 'a t -> int -> 'a
    val length : 'a t -> int
    val back   : 'a t -> 'a
    val copy : 'a t -> 'a t
    val push_back : 'a t -> 'a -> unit
    val iteri : (int -> 'a -> unit) -> 'a t -> unit
    val mapi  : (int -> 'a -> 'b)   -> 'a t -> 'b t
    val iter  : ('a -> unit)        -> 'a t -> unit
    val map   : ('a -> 'b)          -> 'a t -> 'b t
    val foldl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val foldr : ('b -> 'a -> 'a) -> 'a -> 'b t -> 'a
    val filter     : ('a -> bool) -> 'a t -> 'a t
    val find_nexti : ('a -> bool) -> int  -> 'a t -> int option
    val find_next  : ('a -> bool) -> int  -> 'a t -> 'a  option
    val findi      : ('a -> bool) -> 'a t ->         int option
    val find       : ('a -> bool) -> 'a t ->         'a  option
    val exists     : ('a -> bool) -> 'a t -> bool
    val forall     : ('a -> bool) -> 'a t -> bool
    val sort : ('a -> 'a -> bool) -> 'a t -> unit
    val to_list : 'a t -> 'a list
  end
module Dyn_array: Dyn_array_sig = struct
  type 'a t = (('a array) ref * int ref)

  let make size default_value = (ref (Array.make size default_value), ref size)
  let make_empty () = (ref [||], ref 0)
  let of_array array = (ref array, ref (Array.length array))
  let check_size size i = if i >= !size || i < 0
                          then raise (Invalid_argument
                                        (string_of_int i     ^ " out of [0, " ^
                                         string_of_int !size ^ "[ bounds"))
  let get (array, size) i   = check_size size i; Array.get !array i
  let set (array, size) i x = check_size size i; Array.set !array i x
  let (>:) = get
  let length (_, size) = !size
  let back array = array >: length array - 1
  let copy (array, size) = (ref (Array.copy !array), ref !size)
  let push_back (array, size) element =
    if Array.length !array > !size
    then  Array.set !array !size element
    else  array := Array.append !array (Array.make (!size + 1) element);
    incr size
  let iteri f (array, size) =
    for i = 0 to !size - 1 do
      f i !array.(i)
    done
  let mapi f array =
    if length array = 0
    then make_empty ()
    else let new_array = make (length array) (f 0 (array >: 0)) in
         for i = 1 to length array - 1 do
           set new_array i (f i (array >: i))
         done;
         new_array
  let iter f = iteri (fun _ -> f)
  let map  f = mapi  (fun _ -> f)
  let foldl binop seed array =
    let acc = ref seed in
    for i = 0 to length array - 1 do
      acc := binop !acc (array >: i)
    done;
    !acc
  let foldr binop seed array =
    let acc = ref seed in
    for i = length array - 1 downto 0 do
      acc := binop (array >: i) !acc
    done;
    !acc
  let filter pred array =
    let acc = make_empty () in
    for i = 0 to length array - 1 do
      if pred (array >: i)
      then push_back acc (array >: i)
    done;
    acc
  let find_nexti pred start array =
    let i = ref start in
    while !i < length array
          && not (pred (array >: !i)) do
      incr i
    done;
    if !i < length array
    then Some !i
    else None
  let find_next pred start array =
    find_nexti pred start array >>= fun i -> Some (get array i)
  let findi pred array = find_nexti pred 0 array
  let find  pred array = find_next  pred 0 array
  let exists pred (array, size) =
    let i = ref 0 in
    while (!i < !size && not (pred !array.(!i))) do
      incr i
    done;
    !i < !size
  let forall pred (array, size) =
    let i = ref 0 in
    while (!i < !size && pred !array.(!i)) do
      incr i
    done;
    !i == !size
  let sort comp array = (* quick and dirty bubble sort *)
    for i = 0 to length array - 2 do
      for j = length array - 2 downto i do
        if comp (array >: j+1) (array >: j) then
          ( let tmp = array >: (j) in
            set array j     (array >: j+1);
            set array (j+1) tmp
          )
      done
    done
  let to_list (array, size) = Array.to_list (Array.sub !array 0 !size)
end

module DA = Dyn_array
let ( >: ) a i = DA.(>:)   a i
let ( /! ) f a = DA.iter   a f
let ( /!-) f a = DA.iteri  a f
let ( /@ ) f a = DA.map    a f (* map *)

let ( /@-) f a = DA.mapi   a f
let ( // ) f a = DA.filter a f (* filter *)


(** ************************************* **)
(** Basic types needed for the recogniser **)
(** ************************************* **)
type 'a symbol = Non_term of string
               | Terminal of ('a option -> bool) * string
type 'a rule = { lhs : string            (* left  hand side *)
               ; rhs : ('a symbol) DA.t} (* right hand side *)
type 'a grammar = { start_symbol : string;
                    rules        : ('a rule) DA.t}
type 'a input = int -> 'a option (* an opaque representation is more reusable *)
type item = { rule  : int
            ; start : int
            ; next  : int }
type earley_state = (item DA.t) DA.t
module String_set =  Set.Make(String)

(** ******************************* **)
(** Listing of all nullable symbols **)
(** ******************************* **)
let is_nullable null_set rule =
  DA.forall (function
              | Terminal _ -> false
              | Non_term s -> String_set.mem s null_set)
            rule.rhs

let nullable_symbols : 'a grammar -> String_set.t =
  (** Get set of all nullable symbols **)
  fun grammar ->
  let update_nullable_set null_set =
    DA.foldl (fun set rule ->  String_set.add rule.lhs set)
             null_set
             (grammar.rules // is_nullable null_set) in
  let set = ref String_set.empty in
  let old = ref String_set.empty in
  while (set := update_nullable_set !set;
         String_set.cardinal !set > String_set.cardinal !old) do
    old := !set
  done;
  !set

(** ********************** **)
(** Finding infinite loops **)
(** ********************** **)
let infinite_loop : 'a grammar -> bool = fun grammar ->
  let null_set : String_set.t = nullable_symbols grammar
    in

  let rules symbol    = grammar.rules // (fun {lhs} -> lhs = symbol)
                                      // is_nullable null_set
                                      /@ (fun {rhs} -> rhs)
    in

  let add_rule        = DA.foldl (fun set -> function
                                  | Terminal _   -> failwith "impossible"
                                  | Non_term sym -> String_set.add sym set)
    in

  let children symbol = DA.foldl add_rule
                                 String_set.empty
                                 (rules symbol)
    in

  let rec aux path symbol =
    if List.mem symbol path
    then true
    else String_set.exists (aux (symbol::path))
                           (children symbol)
  in String_set.exists (aux []) null_set



(** *********************************** **)
(** Short hands for the main algorithms **)
(** *********************************** **)
let rule_name   grammar index = (grammar.rules >: index).lhs
let rule_body   grammar index = (grammar.rules >: index).rhs
let next_symbol grammar {rule; next} = let rule = (grammar.rules >: rule).rhs in
                                       if DA.length rule > next
                                       then Some (rule >: next)
                                       else None
let append item_set item = if DA.forall ((<>) item) item_set
                           then DA.push_back item_set item

(** ****************************************** **)
(** Earley recogniser                          **)
(**                                            **)
(** You'd better read and understand all of it **)
(** ****************************************** **)
let build_items grammar input =
  let predict s i j nullable_set symbol =
    grammar.rules /!- (fun k {lhs} -> if lhs = symbol then
                                        append (s >: i) { rule  = k
                                                        ; start = i
                                                        ; next  = 0 });
    if String_set.mem symbol nullable_set then (* magic completion *)
      append (s >: i) { (s >: i >: j) with
                        next = (s >: i >: j).next + 1 }

  and scan s i j symbol =
    if symbol (input i)
    then (if DA.length s - 1 <= i then
            DA.push_back s (DA.make_empty ());
          DA.push_back (s >: i + 1) { (s >: i >: j) with
                                      next = (s >: i >: j).next + 1 })

  and complete s i j =
    let complete_item = s >: i >: j in
    (s >: complete_item.start) /! (fun item ->
                                   match next_symbol grammar item with
                                   | None                 -> ()
                                   | Some (Terminal _   ) -> ()
                                   | Some (Non_term name) ->
                                      if name = rule_name grammar
                                                          complete_item.rule
                                      then append (s >: i)
                                                  { item with
                                                    next = item.next +1 })
  in
  let nullable_set = nullable_symbols grammar    in
  let s            = DA.make 1 (DA.make_empty()) in
  (* Seed s with the start symbol *)
  grammar.rules /!- (fun i {lhs} ->
                     if lhs = grammar.start_symbol then
                       DA.push_back (s >: 0) { rule  = i
                                             ; start = 0
                                             ; next  = 0 });
  let i = ref 0 in
  while !i < DA.length s do
    let j = ref 0 in
    while !j < DA.length (s >: !i) do
      (match next_symbol grammar (s >: !i >: !j) with
       | None                      -> complete s !i !j;
       | Some (Non_term name     ) -> predict  s !i !j nullable_set name;
       | Some (Terminal (term, _)) -> scan     s !i !j term;
      );
      incr j
    done;
    incr i
  done;
  s

(** ************************************** **)
(** Post-processing of the Earley items    **)
(**                                        **)
(** Helps when constructing the parse tree **)
(** ************************************** **)
type edge = { rule   : int
            ; finish : int }

let chart_of_items grammar s =
  let chart = s /@ (fun _ -> DA.make_empty ()) in
  s /!- (fun i ->
         DA.iter (fun item -> match next_symbol grammar item with
                              | Some _ -> () (* incomplete items don't apply *)
                              | None   -> DA.push_back
                                            (chart >: item.start)
                                            { rule = item.rule; finish = i }));
  chart /! (fun edges -> DA.sort (fun e1 e2 -> if e1.rule = e2.rule
                                               then e1.finish > e2.finish
                                               else e1.rule < e2.rule)
                                 edges);
  chart

(** ********************************* **)
(** Actual parse tree construction    **)
(** ********************************* **)
let opt_find     f = DA.foldl (function None -> f | e -> const e) None
let opt_find_mem f = opt_find (fun a -> f a >>= (fun out -> Some (a, out)))

let df_search (edges : int -> 'a -> 'b DA.t)
              (child : int -> 'b -> 'a     )
              (pred  : int -> 'a -> bool   )
              (root  : 'a                  )
    : ('a * 'b) list option =
  let rec aux depth root =
    if pred depth root
    then Some []
    else opt_find_mem (child depth |- aux (depth + 1))
                      (edges depth root)
         >>= (fun (edge, path) -> Some ((root, edge) :: path))
  in aux 0 root

let top_list (grammar        : 'a grammar     )
             (input          : 'a input       )
             (chart          : edge DA.t DA.t )
             (start          : int            )
             ({finish; rule} : edge           )
    : (int * edge) list =
  let symbols           = rule_body grammar rule           in
  let bottom            = DA.length symbols                in
  let pred  depth start = depth = bottom && start = finish in
  let child depth edge  = edge.finish                      in
  let edges depth start =
    if depth >= DA.length symbols
    then DA.make_empty ()  (* Going past the maximum depth fails the search *)
    else match symbols >: depth with
         | Terminal (t, _) -> if t (input start)
                              then DA.make 1 {finish = start + 1; rule = -1}
                              else DA.make_empty () (* Non-matching token fail
                                                       the search *)
         | Non_term name   -> (chart >: start) // (fun {finish; rule} ->
                                                   rule_name grammar rule = name)
  in
  match df_search edges child pred start with
  | None      -> failwith "there's allways a solution"
  | Some path -> path

type 'a parse_tree = Token of 'a
                   | Node  of int * 'a parse_tree list

let parse_tree (grammar : 'a grammar    )
               (input   : 'a input      )
               (chart   : edge DA.t DA.t)
    : 'a option parse_tree =
  let start  = 0                                        in
  let finish = DA.length chart - 1                      in
  let name   = grammar.start_symbol                     in
  let rule_name {finish; rule} = rule_name grammar rule in
  let rec aux (start, edge)    =
    if edge.rule = -1
    then Token (input start)
    else Node (edge.rule,
               List.map aux (top_list grammar input chart start edge))
  in
  match DA.find (fun edge -> edge.finish = finish && rule_name edge = name)
                (chart >: start)
        >>= fun edge -> Some (aux (start, edge))
  with
  | None      -> failwith "Are you sure this parse succeeded?"
  | Some node -> node


(** **************** **)
(** Semantic actions **)
(** **************** **)
type sexpr = Nil
           | Int    of int
           | Char   of char
           | String of string
           | List   of sexpr list

type semantic_action = sexpr list -> sexpr

let act (token_handler : 'a -> sexpr         )
        (actions       : semantic_action DA.t)
        (ast           : 'a option parse_tree)
    : sexpr =
  let rec aux = function
    | Token (Some t)    -> token_handler t
    | Token None        -> Nil
    | Node (rule, subs) -> (actions >: rule) (List.map aux subs)
  in aux ast


(** **************************** **)
(** Printing utilities           **)
(**                              **)
(** Really useful when debugging **)
(** **************************** **)
type pretty_printer = { write : string -> unit
                      ; col   : unit   -> unit
                      ; line  : unit   -> unit
                      ; print : unit   -> unit
                      }

let pretty_printer indent =
  let self = DA.make_empty () in
  { write = (fun s -> let line = DA.back self in
                      DA.set line
                             (DA.length line - 1)
                             (DA.back line ^ s)
            )
  ; col   = (fun () -> DA.push_back (DA.back self) "")
  ; line  = (fun () -> DA.push_back self (DA.make_empty ()))
  ; print = let max f      = DA.foldr (f |- max) 0 self in
            let len i v    = if i >= DA.length v
                             then 0
                             else String.length (v >: i)       in
            fun () ->
            for i = 0 to max DA.length - 1 do
              let max_len = max (len i) in
              self /! (fun line ->
                       if i = DA.length line
                       then DA.push_back line "";
                       DA.set
                         line i
                         ((line >: i) ^ String.make
                                          (max_len - String.length(line >: i))
                                          ' '))
            done;
            self /! (fun line -> print_string (String.make indent ' ');
                                 DA.iter print_string line;
                                 print_newline ())
  }

let print_items grammar s =
  s /!- (fun i items ->
         print_endline ("    === " ^ string_of_int i ^ " ===");
         let pp = pretty_printer 4 in
         items /!- (fun i (item:item) ->
                    pp.line ();
                    pp.col (); pp.write(rule_name grammar item.rule);
                    pp.col (); pp.write " ->";
                    let rule = (grammar.rules >: item.rule).rhs in
                    rule /!- (fun i symbol ->
                              if i = item.next then   pp.write " •";
                              match symbol with
                              | Non_term name      -> pp.write (" " ^ name)
                              | Terminal (_, desc) -> pp.write (" " ^ desc));
                    if item.next = DA.length rule then pp.write " •";
                    pp.col(); pp.write ("  (" ^ string_of_int item.start ^ ")"));
         pp.print ();
         print_newline ())

let print_chart grammar s = (* shamelessly copy-pasted from the above *)
  s /!- (fun i items ->
         print_endline ("    === " ^ string_of_int i ^ " ===");
         let pp = pretty_printer 4 in
         items /!- (fun i (item:edge) ->
                    pp.line ();
                    pp.col (); pp.write((grammar.rules >: item.rule).lhs);
                    pp.col (); pp.write " ->";
                    let rule = (grammar.rules >: item.rule).rhs in
                    rule /!- (fun i symbol ->
                              match symbol with
                              | Non_term name      -> pp.write (" " ^ name)
                              | Terminal (_, desc) -> pp.write (" " ^ desc));
                    pp.col(); pp.write ("  ("^ string_of_int item.finish^ ")"));
         pp.print ();
         print_newline ())

(** ********************************************************** **)
(** Cannonical terminal symbols                                **)
(**                                                            **)
(** Terminal symbols can match any value (not just characters) **)
(** Those are the one typically used when parsing plain text   **)
(** ********************************************************** **)
let on_option f = function
  | None   -> false
  | Some x -> f x
let end_of_input : 'a symbol = Terminal ((function None   -> true
                                                 | Some _ -> false)
                                        , "<end>")
let char : char   -> char symbol = fun c -> Terminal
                                              (on_option ((=) c),
                                               "'" ^ String.make 1 c ^ "'")
let alt  : string -> char symbol = fun s -> Terminal (on_option
                                                        (String.contains s),
                                                      "[" ^ s ^ "]")
let range: string -> char symbol = fun s ->
  match String.length s with
  | 2 -> Terminal ( on_option (fun c -> s.[0] <= c && c <= s.[1])
                  , "[" ^ String.sub s 0 1 ^ "-" ^ String.sub s 1 1 ^ "]")
  | _ -> raise (Invalid_argument ("Range: invalid string: \"" ^ s ^ "\""))

let input_of_string s i = if i >= String.length s
                          then None
                          else Some s.[i]


(** ************************* **)
(** Actual grammars and tests **)
(** ************************* **)
let grammar1 =
  let a = DA.of_array                             in
  let n = fun symbol -> Non_term symbol           in
  let r = fun lhs rhs -> {lhs = lhs; rhs = a rhs} in
  { start_symbol = "Sum"
  ; rules =
      a[| r "Sum"     [| n"Sum"    ; alt "+-"; n"Product" |]
        ; r "Sum"     [| n"Product"                       |]
        ; r "Product" [| n"Product"; alt "*/"; n"Factor"  |]
        ; r "Product" [| n"Factor"                        |]
        ; r "Factor"  [| char '(' ; n"Sum"; char ')'      |]
        ; r "Factor"  [| n"Number"                        |]
        ; r "Number"  [| n"Number" ; range "09"           |]
        ; r "Number"  [| range "09";                      |]
       |]
  }

let input1 = input_of_string "1+(2*3+4)"
let s1     = build_items    grammar1 input1
(* let _      = print_items    grammar1 s1 *)
let c1     = chart_of_items grammar1 s1
(* let _      = print_chart    grammar1 c1 *)
let pt     = parse_tree     grammar1 input1 c1

let input_handler c = Char c
let actions =
  DA.of_array
    [| (fun [Int left; Char op; Int right] ->
        Int ((if op == '+' then (+) else (-)) left right))
     ; (fun [Int i] -> Int i)
     ; (fun [Int left; Char op; Int right] ->
        Int ((if op == '*' then ( * ) else (/)) left right))
     ; (fun [Int i] -> Int i)
     ; (fun [_; Int i ;_] -> Int i)
     ; (fun [Int i] -> Int i)
     ; (fun [Int i; Char c] ->
        Int ((i * 10) + int_of_char c - int_of_char '0'))
     ; (fun [Char c] -> Int (int_of_char c - int_of_char '0'))
      |]

let result = act input_handler actions pt

let actions_bis =
  DA.of_array
    [| (fun [left; op; right] -> List [left; op; right])
     ; (fun [i] -> i)
     ; (fun [left; op; right] -> List [left; op; right])
     ; (fun [i] -> i)
     ; (fun [_; i ;_] -> i)
     ; (fun [i] -> i)
     ; (fun [Int i; Char c] ->
        Int ((i * 10) + int_of_char c - int_of_char '0'))
     ; (fun [Char c] -> Int (int_of_char c - int_of_char '0'))
      |]
let result_bis = act input_handler actions_bis pt

let actions_ter =
  DA.of_array
    [| (fun [_; Char op; _] -> print_char op; print_char ' '; Nil)
     ; (fun [_]             ->                                Nil)
     ; (fun [_; Char op; _] -> print_char op; print_char ' '; Nil)
     ; (fun [_]             ->                                Nil)
     ; (fun [_; _ ;_]       ->                                Nil)
     ; (fun [_]             ->                                Nil)
     ; (fun [_; Char c]     -> print_char c;                  Nil)
     ; (fun [Char c]        -> print_char c;  print_char ' '; Nil)
  |]
let result_ter = act input_handler actions_ter pt

(*
let _ = List
          [ Int 1
          ; Char '+'
          ; List [List [ Int 2
                       ; Char '*'
                       ; Int 3]
                 ; Char '+'
                 ; Int 4]]
*)




let grammar2 =
  let a = DA.of_array                             in
  let n = fun symbol -> Non_term symbol           in
  let r = fun lhs rhs -> {lhs = lhs; rhs = a rhs} in
  { start_symbol = "Block"
  ; rules =
      a[| r "Block" [| char ';'                               |]
        ; r "Block" [| n"If"                                  |]
        ; r "If"    [| char 'i'; n"Block"                     |]
        ; r "If"    [| char 'i'; n"Block"; char 'e'; n"Block" |]
       |]
  }

let input2 = input_of_string "ii;e;"
let s2     = build_items    grammar2 input2
(* let _      = print_items    grammar2 s2 *)
let c2     = chart_of_items grammar2 s2
(* let _      = print_chart    grammar2 c2 *)
let pt2    = parse_tree     grammar2 input2 c2



(* this gramar will blow up in your face *)
let grammar3 =
  let a = DA.of_array                             in
  let n = fun symbol -> Non_term symbol           in
  let r = fun lhs rhs -> {lhs = lhs; rhs = a rhs} in
  { start_symbol = "A"
  ; rules =
      a[| r "A" [| n"A" |]
        ; r "A" [|      |]
       |]
  }

let input3 = input_of_string ""
let s3     = build_items    grammar3 input3
(* let _      = print_items    grammar3 s3 *)
let c3     = chart_of_items grammar3 s3
(* let _      = print_chart    grammar3 c3 *)
let _      = if infinite_loop grammar3
             then print_endline "Infinite loop detected! Beware!"
let pt3    = parse_tree     grammar3 input3 c3 (* Stack overflow! *)
