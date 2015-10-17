type expr 
  = Const of int
  | Add of expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr
  | Let of string * expr * expr
  | Var of string
let rec show_expr (ex:expr) : string = 
  match ex with 
  | Const const -> string_of_int const
  | Add (left,right) -> "("^ (show_expr left)^"+"^(show_expr right)^")"
  | Mul (left,right) -> "("^ (show_expr left)^"*"^(show_expr right)^")"
  | Sub (left,right) -> "("^ (show_expr left)^"-"^(show_expr right)^")"
  | Div (left,right) -> "("^ (show_expr left)^"/"^(show_expr right)^")"
  | Let (left,mid,right) -> "(let "^left^" = "^ (show_expr mid)^" in "^(show_expr right)^")"
  | Var str -> str

let rec show_pretty_expr (ex:expr) : string =
  let precedence exp (up:int) = match exp with (*Add,Sub are level 1; Div and Mul are level 2; and Const, Var Let are level 0*)
    | Add (l,r) -> if up > 1 then 1 else if up=1 then 2 else 3
    | Sub (l,r) -> if up > 1 then 1 else if up=1 then 2 else 3 
    | Div (l,r) -> if up < 2 then 3 else 2
    | Mul (l,r) -> if up=2 then 2 else 3
    | Const _ -> if up=0 then 2 else 1
    | Var _ -> if up=0 then 2 else 1
    | Let (_,_,_)-> if up=0 then 2 else 1
  in 
  let childRL exp up :bool = match exp with (*true-> left, false -> right*)
    | Add(_,_) -> if 1=up then false else true
    | Sub(_,_) -> if 2=up then false else true
    | Mul(_,_) -> if 3=up then false else true
    | Div(_,_) -> if 4=up then false else true
    | Let(_,_,_) -> if 5=up then false else true
    | Const _ -> true
    | Var _ -> true
  in
  let rec pp exp prec isLeft blockedatright = 
    if prec=1 then  (*prev prec higher -> 1, equal -> 2, lower -> 3*) 
      match exp with 
      | Const const -> string_of_int const
      | Add (left,right) -> "("^ (pp left (precedence left 1) true false)^"+"^(pp right (precedence right 1) (childRL right 1) true)^")"
      | Sub (left,right) -> "("^ (pp left (precedence left 1) true false)^"-"^(pp right (precedence right 1) (childRL right 2) true)^")"
      | Let (left,mid,right) -> if blockedatright then  "let "^left^" = "^ (pp mid (precedence mid 0) true true)^" in "^(pp right (precedence right 0) (childRL right 5) true)
      				else "(let "^left^" = "^ (pp mid (precedence mid 0) true false)^" in "^(pp right (precedence right 0) (childRL right 5) true)^")"
      | Var str -> str	
    else if prec=2 then(*precedence are equal*) 
      if  isLeft then match exp with  
      	| Const const -> string_of_int const
        | Add (left,right) -> (pp left (precedence left 1) true false)^"+"^(pp right (precedence right 1) (childRL right 1) false)
        | Sub (left,right) -> (pp left (precedence left 1) true false)^"-"^(pp right (precedence right 1) (childRL right 2) false)
        | Mul (left,right) -> (pp left (precedence left 2) true false)^"*"^(pp right (precedence right 2) (childRL right 3) false)
        | Div (left,right) -> (pp left (precedence left 2) true false)^"/"^(pp right (precedence right 2) (childRL right 4) false)
        | Let (left,mid,right) -> if blockedatright then "let "^left^" = "^ (pp mid (precedence mid 0) true true)^" in "^(pp right (precedence right 0) (childRL right 5) true)
                                  else "(let "^left^" = "^ (pp mid (precedence mid 0) true true)^" in "^(pp right (precedence right 0) (childRL right 5) true)^")"
        | Var str -> str
      else match exp with 
        | Const const -> string_of_int const
        | Add (left,right) -> "("^ (pp left (precedence left 1) true false)^"+"^(pp right (precedence right 1) (childRL right 1) true)^")"
	| Sub (left,right) -> "("^ (pp left (precedence left 1) true false)^"-"^(pp right (precedence right 1) (childRL right 2) true)^")"
        | Mul (left,right) -> "("^ (pp left (precedence left 2) true false)^"*"^(pp right (precedence right 2) (childRL right 3) true)^")"
        | Div (left,right) -> "("^ (pp left (precedence left 2) true false)^"/"^(pp right (precedence right 2) (childRL right 4) true)^")"
        | Let (left,mid,right) ->if blockedatright then "let "^left^" = "^ (pp mid (precedence mid 0) true true)^" in "^(pp right (precedence right 0) (childRL right 5) true)
				  else "(let "^left^" = "^ (pp mid (precedence mid 0) true true)^" in "^(pp right (precedence right 0) (childRL right 5) true)^")"
	| Var str -> str
    else match exp with 
      | Const const -> string_of_int const
      | Add (left,right) -> (pp left (precedence left 1) true false)^"+"^(pp right (precedence right 1) (childRL right 1) true)
      | Sub (left,right) -> (pp left (precedence left 1) true false)^"-"^(pp right (precedence right 1) (childRL right 2) true)
      | Mul (left,right) -> (pp left (precedence left 2) true false)^"*"^(pp right (precedence right 2) (childRL right 3) true)
      | Div (left,right) -> (pp left (precedence left 2) true false)^"/"^(pp right (precedence right 2) (childRL right 4) true)
      | Let (left,mid,right) -> if blockedatright then "let "^left^" = "^ (pp mid (precedence mid 0) true true)^" in "^(pp right (precedence right 0) (childRL right 5) true)
                                  else "(let "^left^" = "^ (pp mid (precedence mid 0) true true)^" in "^(pp right (precedence right 0) (childRL right 5) true)^")" 
      | Var str -> str	
    in 
    pp ex (precedence ex (0)) true true

