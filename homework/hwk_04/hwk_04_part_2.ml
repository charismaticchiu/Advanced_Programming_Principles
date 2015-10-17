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
    | Add (l,r) -> eval_h l env free @ eval_h r env free (* this is more concise than Sub,Mul,Div*)
    | Sub (l,r) ->
        let v1 = eval_h l env free and  v2 = eval_h r env free
        in (match v1,v2 with
            | [],[] -> []
            | _ -> v1@v2)
    | Mul (l,r) ->
        let v1 = eval_h l env free and  v2 = eval_h r env free
        in (match v1,v2 with
            | [],[] -> []
            | _ -> v1@v2)
    | Div (l,r) ->
        let v1 = eval_h l env free and  v2 = eval_h r env free
        in (match v1,v2 with
            | [],[] -> []
            | _ -> v1@v2)
    | Let (l,m,r) -> let v1 = eval_h m env free
                     in
                     let env=env@[l]in (v1@eval_h r env free)
    | Var v ->  if lookup v env  then [] else [v]  
    | LT (l,r) -> let v1 = eval_h l env free and  v2 = eval_h r env free
        in (match v1,v2 with
            |[],[] -> []
            | _,_ -> v1@v2)
    | IfThenElse (l,m,r) -> let v1 = eval_h l env free
        in (
        if (match v1 with |[] ->true | _ -> true)
        then ((v1@eval_h m env free))
        else ((v1@eval_h l env free)))
    | EQ (l,r) ->let v1 = eval_h l env free and v2 = eval_h r env free
        in ( match v1,v2 with
        | [],[] -> []
        | _,_ -> v1@v2)
    | And (l,r) -> let v1 = eval_h l env free and  v2 = eval_h r env free
        in (match v1,v2 with
        | [], [] -> []
        | _,_-> v1@v2)
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
  let rec bool_h ex= match ex with 
	| LT_bool (a,b) -> if (int_h a) < (int_h b) then true else false
      	| EQ_int_bool(a,b)-> if (int_h a)=(int_h b) then true else false
	|EQ_bool_bool(a,b)-> if (bool_h a)=(bool_h b)then true else false
	| And_bool (a,b)-> (bool_h a)&&(bool_h b)
	| Not_bool a -> if (bool_h a) then false else true
  	| BoolConst_bool b -> b
  	| IfThenElse_bool (a,b,c)-> if (bool_h a) then (bool_h b) else (bool_h c)
  	| Var_bool a -> true (*cheat*) 
  	| Let_bool_bool (a,b,c)-> let a = (bool_h b) in (bool_h c)
  	| Let_int_bool (a,b,c)-> let a = (int_h b) in (bool_h c)
      and int_h ex= match ex with 
	| IntConst_int a -> a 
	| Add_int (a,b) -> (int_h a) + (int_h b)
	| Sub_int (a,b) -> (int_h a) - (int_h b)
  	| Mul_int (a,b) -> (int_h a) * (int_h b)
	| Div_int (a,b) -> if (int_h b)=0 then raise(Failure "Division_by_zero") else (int_h a) / (int_h b)
	| Var_int a ->  1 (*cheat*)
	| Let_int_int (a,b,c) -> let a = (int_h b) in (int_h c)
	|Let_bool_int (a,b,c) -> let a = (bool_h b) in (int_h c)
	|IfThenElse_int(a,b,c)-> if (bool_h a) then (int_h b) else (int_h c)
  in
  match ex with 
  |BoolExpr boolex -> BoolVal (bool_h boolex)
  |IntExpr intex -> IntVal (int_h intex)

let eval2 (e:expr) :int_or_bool_expr =
  let rec lookup n env =
    match env with
    | (name,value)::rest -> if n = name then value else lookup n rest
  and
  eval_int (e:expr) = match e with 
	       | IntConst v -> IntConst_int v
	       | Add (l,r) -> let v1 = eval_int l and v2 = eval_int r
        	              in Add_int (v1,v2 )           
    	       | Sub (l,r) -> let v1 = eval_int l and v2 = eval_int r
        		      in Sub_int (v1,v2)            
    	       | Mul (l,r) -> let v1 = eval_int l and v2 = eval_int r
                              in Mul_int (v1,v2)           
    	       | Div (l,r) -> let v1 = eval_int l and v2 = eval_int r
                              in Div_int (v1,v2)    
	       | Let(l,m,r)-> (match m,r with
			      |BoolConst _, IntConst _ -> let v1 = eval_bool m and v2 = eval_int r in Let_bool_int (l,v1,v2)
  			      | IntConst _, IntConst _ -> let v1 = eval_int m and v2 = eval_int r in Let_int_int (l,v1,v2)
	       		      | _ -> None)
	       | Var v -> Var_int v
	       |IfThenElse (l,m,r) -> ( match m,r with
                               | IntConst _ , IntConst _ -> let v1 = eval_bool l and v2 = eval_int m and v3 =eval_int r in IfThenElse_int (v1,v2,v3)
                               | _ -> None) 
  
  and
  eval_bool (e:expr) = match e with 
		| BoolConst b -> BoolConst_bool b
  		| Let(l,m,r)->( match m,r with 
			       | IntConst _ , BoolConst _ -> let v1 = eval_int m and v2 = eval_bool r in Let_int_bool (l,v1,v2) 
			       | BoolConst _, BoolConst _ ->let v1 = eval_bool m and v2 = eval_bool r in Let_bool_bool (l,v1,v2)
			       | _ -> None)
		| LT(l,r) ->( match l,r with 
			      | IntConst a, BoolConst b -> None
			      | BoolConst a, IntConst b -> None
			      | BoolConst a,BoolConst b-> None
			      | _ -> let v1 = eval_int l and v2 = eval_int r in LT_bool(v1,v2))
		| EQ(l,r) ->( match l,r with 
			     | IntConst a,IntConst b -> let v1 = eval_int l and v2 = eval_int r in EQ_int_bool (v1,v2)
			     | BoolConst a,BoolConst b -> let v1 = eval_bool l and v2 = eval_bool r in EQ_bool_bool (v1,v2)
			     | _ -> None	)
		| And(l,r)->let v1 = eval_bool l and v2 = eval_bool r in And_bool (v1,v2)
		| Not(v) -> let v1 = eval_bool v in Not_bool (v1)
		| IfThenElse(l,m,r)-> ( match m,r with
                               | BoolConst _, BoolConst _ ->let v1 = eval_bool l and v2 = eval_bool m and v3 = eval_bool r in IfThenElse_bool (v1,v2,v3)
                               | _ -> None)
  in
  match e with 
  | Add(l,r) -> IntExpr (eval_int e)
  | Sub(l,r) -> IntExpr (eval_int e)
  | Mul(l,r) -> IntExpr (eval_int e)
  | Div(l,r) -> IntExpr (eval_int e)
  | Let(l,m,r)-> ( match m,r with (*remember to add parentheses for the reason of scope*)
		|IntConst a, IntConst b ->IntExpr (eval_int e)
		|IntConst a, BoolConst b->BoolExpr (eval_bool e)
  		|BoolConst a,BoolConst b->BoolExpr (eval_bool e)
		|BoolConst a,IntConst b ->IntExpr (eval_int e))
  (*| Var v -> let v1= lookup v env 
	     in(
	     match v1 with 
	     | BoolConst_bool b -> BoolExpr(eval_bool e) 
	     | IntConst_int i -> IntExpr(eval_int e)*)
  | LT(l,r) -> BoolExpr(eval_bool e)  
  | IfThenElse (l,m,r) -> ( 
	match (l,m,r) with
	| _,BoolConst b,_ -> BoolExpr(eval_bool e)
	| _ -> IntExpr(eval_int e))	
  | EQ (l,r) -> BoolExpr(eval_bool e)  
  | And (l,r) -> BoolExpr(eval_bool e)  
  | Not l -> BoolExpr(eval_bool e)  

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
  | _ -> None

