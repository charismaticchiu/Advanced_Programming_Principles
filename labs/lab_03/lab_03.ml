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


