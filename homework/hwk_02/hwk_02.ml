let even num = if num mod 2 = 1 then false else true
let rec euclid a b = if a = b then a 
	else if a < b then euclid a (b-a)
	else euclid (a-b) b
let frac_add (a1,b1) (a2,b2) = (a1*b2+a2*b1,b1*b2)
let frac_simplify (a,b) = let gcd = euclid a b in ((a/gcd,b/gcd))
let rec max_list (l:int list) = match (l:int list) with (* remember the type usage to create "int list -> int" *)
	| (hd:int)::(tl:int)::[] -> if hd > tl then hd else tl (* must wirte before the following line *)
	| (hd:int)::(mid:int )::(tl:int list) -> if hd > mid then max_list (hd::tl) else max_list (mid::tl)
let rec drop (num) (l) = 
	if num <= 0 then l
	else match l with
	| [] -> []
	| tl::[] -> []
	| hd::tl::[] -> drop (num-1) (tl::[]) 	(* remember to put the parenthesis and "::[]" to "tl" so it can suit the type "list" *)
	| (hd)::(mid)::(tl) -> drop (num-1) (mid::tl)
let rec num_of_ele l inc=(* let inc start from 0 always ; just a expdiency*)
	match l with 
	| [] -> inc
	| hd::tl::[] -> inc + 2
	| hd::mid::tl -> num_of_ele (mid::tl) inc+1
let rec rev l = let eleNum = num_of_ele l 0 in (* num_of_ele is designed to be counted from 0 *)
	let rec helper m num_last newList = (* m is the original list that element will be taken out 1-by-1; newList is the list that element will be put in *) 
		if num_last = 0 then newList else
		match m with 
		| [] -> []
		| hd::tl -> helper (tl) (num_last-1) ([hd] @ newList)
	in helper l eleNum []
let rec approx_squareroot accr n = 
	let rec helper upper lower guess target = 
	if upper -. lower > accr then  if (guess*.guess) > target 
					then  helper guess lower ((lower+.upper)/.2.0) target
					else  helper upper guess ((lower+.upper)/.2.0) target
	else (lower,upper)
	in helper n 1.0 ((1.0+.n)/.2.0) n
let rec is_matrix mat = match mat with 
	| [] -> true
	| hd::[] -> true
	| hd::tl::[] -> if num_of_ele hd 0 = num_of_ele tl 0 then true 
			else false
	| hd::tl -> let rec helper mat2 dim = match mat2 with (* dim refers to dimension*)
				| hd::tl -> if dim = num_of_ele hd 0 then helper tl dim else false 
	in helper tl (num_of_ele hd 0)
let rec matrix_scalar_add mat num = let rec helperCol colLeft scalar =  match colLeft with                
		| [] -> []
		| tl2::[] -> [tl2+scalar]  
                | hd2::mid::tl2 -> (hd2+scalar)::(helperCol (mid::tl2) scalar)                                                       
                in let rec helperRow matLeft num = match matLeft with
			| []->[[]]
                	| tl3::[] -> [helperCol tl3 num]
                	| hd3::tl3 -> [(helperCol hd3 num)]@(helperRow tl3 num)
		in  helperRow mat num
let rec map (f:'a -> 'b) (l: 'a list) :'b list =(* remember this implementation*)
        match l with
        | [] -> []
        |  hd::tl ->  f hd :: map f tl
let find_head l = match l with
        | hd::tl -> hd
        | [] -> raise (Failure "oops")
let rec find_tail l = match l with
		| hd::[] -> hd
        | hd::tl -> find_tail tl 
        | [] -> raise (Failure "oops")
(*let rec transpose list = match list with
| []             -> []
| []   :: xss    -> transpose xss
| (x::xs) :: xss -> (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)
let rec matrix_transpose mat = match mat with 
	|[] -> []
	|[]:: x -> matrix_transpose x
	|x::xs::xss -> (x :: map find_head xss) :: matrix_transpose (xs :: map find_tail xss)
*)
