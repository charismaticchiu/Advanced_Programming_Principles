type formula = And of formula * formula
         | Or  of formula * formula
         | Not of formula 
         | Prop of string
         | True
         | False
type subst = (string * bool) list
exception KeepLooking
let show_int_int_pair (x,y) = "(" ^ (Int.to_string x) ^ "," ^ (Int.to_string y) ^ ")"
  
let show_list show l =
  let rec sl l =
    match l with 
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl l ^ " ]"

let show_string_bool_pair (s,b) =
  "(\"" ^ s ^ "\"," ^ (if b then "true" else "false") ^ ")"

let show_subst = show_list show_string_bool_pair
let rec find var env =
  match env with
  | [] -> false
  | (name,value)::xs -> if var = name then value else find var xs

let rec eval (e:formula) (ref:subst): bool = 
  match e with 
  | And (l,r)-> (eval l ref) && (eval r ref) 
  | Or  (l,r)-> (eval l ref) || (eval r ref)
  | Not v    -> not(eval v ref)
  | Prop v   -> find v ref
  | True -> true
  | False -> false
assert (eval (And ( Prop "P", Prop "Q")) [("P",true); ("Q",false)] = false )
assert (eval (And ( Prop "P", Prop "Q")) [("P",true); ("Q",true)] = true )
(*let rec is_elem (var:string) (env:string list): bool = 
  match env with 
  | [] -> false
  | x::xs -> if x=var then true else is_elem var xs*)
(**)
let freevars (e:formula): string list =
  let rec help (e:formula) (acc:string list): string list = 
    match e with
    | And (l,r) 
    | Or  (l,r)-> (help l acc) @ (help r acc)
    | Not v    ->  help v acc
    | Prop v   -> [v] @ acc(*if is_elem v acc then acc else [v]@acc*)
    | True | False -> acc
  in List.dedup (help e [])
assert ( List.length (freevars (And ( Not (Prop "Z"), Or (Prop "Q", And (Prop "P", Prop "S"))))) = 4 )
(*include all the possibilities*)
let gen_env (lst:string list) : (subst list) =
  let rec subsets partial_subset rest_of_the_set =
    match rest_of_the_set with
    | [] -> [ partial_subset ]
    | x::xs -> (subsets (partial_subset @ [x,true]) xs)
	       @
               (subsets (partial_subset @ [x,false]) xs)
  in subsets [] lst 
let is_tautology (e:formula) (f:subst -> subst option):subst option =
  let rec help e env =
    match env with 
    | [] -> None
    | x::xs ->  match eval e x with
		| true -> help e xs 
		| false-> try (f x) with KeepLooking -> help e xs(**if false then apply the function on x*)
  in try  (help e (gen_env (freevars e))) with KeepLooking -> None
let is_tautology_first f = is_tautology f (fun s -> Some s)
let is_tautology_print_all f =
  is_tautology 
    f
    (fun s -> print_endline (show_subst s); 
          raise KeepLooking)
exception Case_doesnt_exist
let maze_move (pos:int*int) : (int*int) list = (*for simplification, never go back to the previous step*)
  match pos with 
  (*|(1,1) -> [(2,1)]
  |(2,1) -> [(3,1)]*)
  |(3,1) -> [3,1](*keeplooking case*)
  (*|(4,1) -> [(4,2)]*)
  |(5,1) -> [5,1](*raise Found*)
  |(1,2) -> [(2,2)]
  |(2,2) -> [(3,2)]
  |(3,2) -> [(3,3);(3,1);(4,2)]
  |(4,2) -> [4,2](*raise KeepLooking(*[(4,1);(3,2)]*)*)
  |(5,2) -> [(5,1)]
  |(1,3) -> [(1,2);(1,4)](*eliminate (2,3); silly to go back*)
  |(2,3) -> [(1,3)]
  |(3,3) -> [(4,3);(3,4)]
  |(4,3) -> [(5,3)]
  |(5,3) -> [(5,2);(5,4)]
  |(1,4) -> [(1,5)]
  |(2,4) -> [(3,4)]
  |(3,4) -> [(4,4);(3,3)]
  |(4,4) -> [(4,5)]
  |(5,4) -> [5,4](*raise KeepLooking(*[(5,3)]*)*)
  |(1,5) -> [(2,5)]
  |(2,5) -> [(2,4)]
  |(3,5) -> [3,5]
  |(4,5) -> [(3,5);(5,5)]
  |(5,5) -> [(5,5)](*keeplooking case*)
  |(_,_) -> raise Case_doesnt_exist
let rec foldr f v l = match l with
  | [] -> v
  | x::xs -> f x (foldr f v xs)
let is_elem v l =
  foldr (fun x in_rest -> if x = v then true else in_rest) false l
let maze (unit_in:unit) : (int * int) list option = (*rule: never go back to the previous step*)
  let rec maze_run (lst:(int*int) list)  (acc:(int*int) list) : (int * int) list option = 
    match lst with 
    |[3,5]|[5,1] -> Some (acc @ lst)
    |[3,1]|[4,2]|[5,4]|[5,5] -> raise KeepLooking(*deadend*)
    | [] -> raise KeepLooking
    |x::[] -> (try maze_run (maze_move x) (acc @ [x]) with KeepLooking -> None)
    |x::xs -> (try maze_run (maze_move x) (acc @ [x]) with KeepLooking -> maze_run xs acc )
  in try  maze_run (maze_move (2,3)) [(2,3)] with KeepLooking -> None

