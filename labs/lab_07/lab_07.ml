type expr 
  = Add of  expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr
  | Let of string * expr * expr
  | Var of string
  | LT of expr * expr
  | EQ of expr * expr
  | And of expr * expr
  | Not of expr
  | IfThenElse of expr * expr * expr
  | IntConst of int
  | BoolConst of bool

type value = IntVal of int | BoolVal of bool
let eval (e:expr) : value =
  let rec lookup n env = 
    match env with 
    | [ ] -> raise (Failure ("Identifier \"" ^ n ^ "\" not declared."))
    | (name,value)::rest -> if n = name then value else lookup n rest
  in
  let rec lookup2 n env =
    match env with
    | [ ] -> BoolVal true
    | (name,value)::rest -> if n = name then value else lookup2 n rest
  in
  let rec eval_h e env = match e with
    | Add (l,r) -> 
        let v1 = eval_h l env  and  v2 = eval_h r env
        in (match v1,v2 with 
            | IntVal x, IntVal y -> IntVal (x + y)
            | _ -> raise (Failure "Addition requires 2 integer values."))
    | Sub (l,r) ->
        let v1 = eval_h l env  and  v2 = eval_h r env
        in (match v1,v2 with
            | IntVal x, IntVal y -> IntVal (x - y)
            | _ -> raise (Failure "Sustraction requires 2 integer values."))
    | Mul (l,r) ->
        let v1 = eval_h l env  and  v2 = eval_h r env
        in (match v1,v2 with
            | IntVal x, IntVal y -> IntVal (x * y)
            | _ -> raise (Failure "Multiplication requires 2 integer values."))
    | Div (l,r) ->
        let v1 = eval_h l env  and  v2 = eval_h r env
        in (match v1,v2 with
            | IntVal x, IntVal y -> IntVal (x / y)
            | _ -> raise (Failure "Division requires 2 integer values."))
    | Let (l,m,r) -> let v1 = eval_h m env 
		     in  eval_h r ((l,v1)::env)(*use concatenation for the scope problem*)
    | Var v -> lookup v env 
    | LT (l,r) -> let v1 = eval_h l env and  v2 = eval_h r env 
	in (match v1,v2 with 
	    |IntVal a, IntVal b -> if a < b then (BoolVal true) else (BoolVal false) 
    	    | _,_ -> raise(Failure ("Less than operator requires 2 integer values.")))
    | IfThenElse (l,m,r) -> let v1 = eval_h l env 
	in (
	if (match v1 with |BoolVal b ->b | _ -> raise (Failure ("It is not a valid expression for Not."))) 
	then (eval_h m env) 
	else (eval_h l env))
    | EQ (l,r) ->let v1 = eval_h l env and  v2 = eval_h r env 
        in ( match v1,v2 with 
	| BoolVal a, BoolVal b -> if a=b then BoolVal true else BoolVal false
	| IntVal a, IntVal b -> if a=b then BoolVal true else BoolVal false
	| _,_ -> raise (Failure ("Equality test requires 2 values of the same type.")))
    | And (l,r) -> let v1 = eval_h l env and  v2 = eval_h r env
        in (match v1,v2 with 
	| BoolVal a, BoolVal b -> BoolVal (a && b)
	| _,_-> raise (Failure ("Equality test requires 2 values of BoolVal.")))
    | Not l -> let v1 = eval_h l env 
	in (match v1 with 
	| BoolVal b -> if (b = true) then BoolVal false else BoolVal true
	| _ -> raise (Failure ("It is not a valid expression for Not.")))
    | IntConst v -> IntVal v
    | BoolConst b -> BoolVal b
  in eval_h e []

let freevars (ex:expr) : string list= 
  let rec lookup n env =
    match env with
    | [ ] -> false
    | name::rest -> n = name || lookup n rest 
  in
  let rec eval_h e env (free:string list)= match e with
    | Add (l,r) | Sub (l,r) | Mul (l,r) | Div (l,r) 
    | LT (l,r) | EQ (l,r) | And (l,r) -> eval_h l env free @ eval_h r env free 
    | Let (l,m,r) -> let v1 = eval_h m env free
                     in( let env=env@[l]in (v1@eval_h r env free))
    | Var v ->  if lookup v env  then [] else [v]  
  (*  | LT (l,r) | EQ (l,r) | And (l,r) -> let v1 = eval_h l env free and  v2 = eval_h r env free
        in (match v1,v2 with
            |[],[] -> []
            | _,_ -> v1@v2) *)
    | IfThenElse (l,m,r) -> let v1 = eval_h l env free
        in (
        if (match v1 with |[] ->true | _ -> true)
        then (v1 @ eval_h m env free)
        else (v1 @ eval_h l env free))
    | Not l -> let v1 = eval_h l env free
        in (match v1 with
        | [] -> []
        | _ -> v1)
    | IntConst v -> []
    | BoolConst b -> []

 in eval_h ex [] []


type int_expr =
  | Add_int of int_expr * int_expr
  | Mul_int of int_expr * int_expr
  | Sub_int of int_expr * int_expr
  | Div_int of int_expr * int_expr
  | IntConst_int of int
  | Var_int of string
  | Let_int_int of string * int_expr * int_expr
  | Let_bool_int of string * bool_expr * int_expr
  | IfThenElse_int of bool_expr * int_expr * int_expr
  | None
 and bool_expr =
  | LT_bool of int_expr * int_expr
  | EQ_int_bool of int_expr * int_expr
  | EQ_bool_bool of bool_expr * bool_expr
  | And_bool of bool_expr * bool_expr
  | Not_bool of bool_expr
  | BoolConst_bool of bool
  | IfThenElse_bool of bool_expr * bool_expr * bool_expr
  | Var_bool of string
  | Let_bool_bool of string * bool_expr * bool_expr
  | Let_int_bool of string * int_expr * bool_expr
  | None
type int_or_bool_expr
  = IntExpr of int_expr
  | BoolExpr of bool_expr
let eval_int_bool (ex:int_or_bool_expr) :value =
  let rec lookup_bool n env =
    match env with
    | [ ] -> false
    | (name,value)::rest -> if n = name then value else lookup_bool n rest 
  in 
  let rec lookup_int n env =
    match env with
    | [ ] -> 0
    | (name,value)::rest -> if n = name then value else lookup_int n rest
 in 
 let rec bool_h ex envi envb :bool= match ex with 
	| LT_bool (a,b) -> (int_h a envi envb) < (int_h b envi envb) 
      	| EQ_int_bool(a,b)-> let v1 = int_h a envi envb and v2 = int_h b envi envb
			     in (match v1 with 
			      	| v2 -> true 
 			      	| _ -> false)
	|EQ_bool_bool(a,b)-> let v1 = bool_h a envi envb and v2 = bool_h b envi envb 
			     in (match v1 with 
			        | v2 -> true 
			        | _ ->  false)
	| And_bool (a,b)-> (bool_h a envi envb)&&(bool_h b envi envb)
	| Not_bool a -> (match  (bool_h a envi envb) with | true -> false | false -> true)
  	| BoolConst_bool b -> b
  	| IfThenElse_bool (a,b,c)-> if (bool_h a envi envb) then (bool_h b envi envb) else (bool_h c envi envb)
  	| Var_bool a -> lookup_bool a envb
  	| Let_bool_bool (a,b,c)-> bool_h c envi ((a,(bool_h b envi envb))::envb)
  	| Let_int_bool (a,b,c)->  bool_h c ((a,int_h b envi envb)::envi) envb
      and int_h ex envi envb : int= match ex with 
	| IntConst_int a -> a 
	| Add_int (a,b) -> (int_h a envi envb) + (int_h b envi envb)
	| Sub_int (a,b) -> (int_h a envi envb) - (int_h b envi envb)
  	| Mul_int (a,b) -> (int_h a envi envb) * (int_h b envi envb)
	| Div_int (a,b) -> (match  (int_h b envi envb) with
			    | 0 -> raise(Failure "Division_by_zero") 
  			    | _ -> (int_h a envi envb) / (int_h b envi envb))
	| Var_int a ->  lookup_int a envi
	| Let_int_int (a,b,c) -> int_h c ((a,(int_h b envi envb))::envi) envb
	|Let_bool_int (a,b,c) -> int_h c envi ((a,(bool_h b envi envb))::envb)
	|IfThenElse_int(a,b,c)-> if (bool_h a envi envb) then (int_h b envi envb) else (int_h c envi envb)
  in
  match ex with 
  |BoolExpr boolex -> BoolVal (bool_h boolex [] [])
  |IntExpr intex -> IntVal (int_h intex [] [])
(*eval2 is the helper function of translate*)
let eval2 (e:expr) :int_or_bool_expr =
  let rec lookup (n:string) (env:string list) =
    match env with
    | [] -> []
    | (name)::rest -> if n = name then [name] else lookup n rest
  and
  eval_int (e:expr) (env:string list) = match e with 
	       | IntConst v -> IntConst_int v
	       | Add (l,r) -> let v1 = eval_int l env and v2 = eval_int r env
        	              in Add_int (v1,v2 )           
    	       | Sub (l,r) -> let v1 = eval_int l env and v2 = eval_int r env
        		      in Sub_int (v1,v2)            
    	       | Mul (l,r) -> let v1 = eval_int l env and v2 = eval_int r env
                              in Mul_int (v1,v2)           
    	       | Div (l,r) -> let v1 = eval_int l env and v2 = eval_int r env
                              in Div_int (v1,v2)    
	       | Let(l,m,r)-> (match m,r with
			      |BoolConst _, IntConst _ -> let v1 = eval_bool m env and v2 = (eval_int r ([l]@env)) in Let_bool_int (l,v1,v2)
  			      | IntConst _, IntConst _ -> let v1 = eval_int m env and v2 = (eval_int r ([l]@env)) in Let_int_int (l,v1,v2)
	       		      | _ -> None)
	       | Var v -> let v1 = lookup v env 
			  in
			  (match v1 with 
			   | [] -> None
			   | _ -> Var_int v)
	       |IfThenElse (l,m,r) -> ( match m,r with
                               | IntConst _ , IntConst _ -> let v1 = eval_bool l env and v2 = eval_int m env and v3 =eval_int r env 
				 			    in IfThenElse_int (v1,v2,v3)
                               | _ -> None) 
  
  and
  eval_bool (e:expr) (env:string list) = match e with 
		| BoolConst b -> BoolConst_bool b
  		| Let(l,m,r)->(match m,r with 
			       | IntConst _ , BoolConst _ -> let v1 = eval_int m env and v2 = (eval_bool r ([l]@env)) in Let_int_bool (l,v1,v2) 
			       | BoolConst _, BoolConst _ ->let v1 = eval_bool m env and v2 = (eval_bool r ([l]@env)) in Let_bool_bool (l,v1,v2)
			       | _ -> None)
		| LT(l,r) ->(match l,r with 
			     | IntConst a, BoolConst b -> None
			     | BoolConst a, IntConst b -> None
			     | BoolConst a,BoolConst b-> None
			     | _ -> let v1 = eval_int l env and v2 = eval_int r env in LT_bool(v1,v2))
		| EQ(l,r) ->(match l,r with 
			     | IntConst a,IntConst b -> let v1 = eval_int l env and v2 = eval_int r env in EQ_int_bool (v1,v2)
			     | BoolConst a,BoolConst b -> let v1 = eval_bool l env and v2 = eval_bool r env in EQ_bool_bool (v1,v2)
			     | _ -> None)
		| Var v -> let v1= lookup v env
                           in
			   (match v1 with
                           | [] -> None
                           | _ -> Var_bool v)
		| And(l,r)->let v1 = eval_bool l env and v2 = eval_bool r env in And_bool (v1,v2)
		| Not(v) -> let v1 = eval_bool v env in Not_bool (v1)
		| IfThenElse(l,m,r)-> ( match m,r with
                               | BoolConst _, BoolConst _ ->let v1 = eval_bool l env and v2 = eval_bool m env and v3 = eval_bool r env in IfThenElse_bool (v1,v2,v3)
                               | _ -> None)
  in
  match e with (*recognize what type it is and pass them into helper function*)
  | Add(l,r) -> IntExpr (eval_int e [])
  | Sub(l,r) -> IntExpr (eval_int e [])
  | Mul(l,r) -> IntExpr (eval_int e [])
  | Div(l,r) -> IntExpr (eval_int e [])
  |Let(l,m,r)->(match m,r with (*remember to add parentheses for the reason of scope*)
		|IntConst a, IntConst b ->IntExpr (eval_int e [])
		|IntConst a, BoolConst b->BoolExpr (eval_bool e [])
  		|BoolConst a,BoolConst b->BoolExpr (eval_bool e [])
		|BoolConst a,IntConst b ->IntExpr (eval_int e []))
  | Var v -> IntExpr(eval_int e [])
  | LT(l,r) -> BoolExpr(eval_bool e [])  
  | IfThenElse (l,m,r) -> ( 
	match (l,m,r) with
	| _,BoolConst b,_ -> BoolExpr(eval_bool e [])
	| _ -> IntExpr(eval_int e []))	
  | EQ (l,r) -> BoolExpr(eval_bool e [])  
  | And (l,r) -> BoolExpr(eval_bool e [])  
  | Not l -> BoolExpr(eval_bool e [])  

let rec translate (ex:expr) : int_or_bool_expr option = 
  let v =(eval2 ex)
  in 
  match v with 
  | BoolExpr a ->( match a with 
		| None -> None 
		| _-> Some (v))
  | IntExpr a ->(match a with 
		| None -> None
		| _ -> Some (v))

