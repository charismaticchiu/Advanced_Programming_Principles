type number = INT of int
        | FLT of float
let n1 = INT 1
let n2 =INT 2
let n4 =INT 4
let n5 = INT 5
let n3_1415 = FLT 3.1415
let to_int i = match i with
        |INT x -> Some x
        |FLT n -> None
let to_float f = match f with
        |FLT n -> Some n
        |INT x -> None
let add_number (x:number) (y:number)= match (x,y) with
        |(INT a,FLT b)->FLT ((float a)+.b)(*remember to add those parentheses*)
        |(INT a,INT b)->INT  (a+b)
        |(FLT a,INT b)->FLT  (a+. (float b))
        |(FLT a,FLT b)->FLT  (a+.b)
let sub_number (x:number) (y:number)= match (x,y) with
        |(INT a,FLT b)->FLT ((float a)-.b)(*remember to add those parentheses*)
        |(INT a,INT b)->INT  (a-b)
        |(FLT a,INT b)->FLT  (a-. (float b))
        |(FLT a,FLT b)->FLT  (a-.b)
let mul_number (x:number) (y:number)= match (x,y) with
        |(INT a,FLT b)->FLT ((float a)*.b)(*remember to add those parentheses*)
        |(INT a,INT b)->INT  (a*b)
        |(FLT a,INT b)->FLT  (a*. (float b))
        |(FLT a,FLT b)->FLT  (a*.b)
let div_number  (x:number) (y:number) = match (x,y) with
        |(INT a,FLT b)-> FLT ((float a)/.b)(*remember to add those parentheses*)
        |(INT a,INT b)-> if (a mod b) = 0 then INT (a/b) else FLT ((float a)/.(float b))
        |(FLT a,INT b)-> FLT  (a/. (float b))
        |(FLT a,FLT b)-> FLT ( a/.b)
let max_number (x:number) (y:number) = match (x,y) with
	|(INT a,FLT b)->if (float a)-.b>0.0 then INT a else FLT b(*remember to add those parentheses*)
        |(INT a,INT b)->if a-b>0 then INT a else INT b
        |(FLT a,INT b)->if a-.(float b)>0.0 then FLT a else INT b
        |(FLT a,FLT b)->if a-.b>0.0 then FLT a else FLT b
let rec sum_number_diffs (l:number list) = match l with 
	| [] -> None
        | x1::[] -> None
        | x1::(x2::[]) ->Some (sub_number x1 x2)
        | x1::x2::tl -> match sum_number_diffs (x2::tl) with 
			|Some s -> Some (add_number (sub_number x1 x2) s)
let rec max_number_list (l:number list) = match l with
	|[] -> None
	|x::[] -> Some x
        |x::xs::[] -> Some (max_number x xs)
	|x::xs::xss ->match max_number_list (xs::xss) with 
			| Some s ->  Some  (max_number (max_number x xs) s)
