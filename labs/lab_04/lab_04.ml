type number = INT of int 
	| FLT of float
let n1 = INT 1
let n2 =INT 2
let n4 =INT 4
let n5 = INT 5
let n3_1415 = FLT 3.1415
let to_int  :number-> int option = function(*number ->int option*)(*if input is an INT then return,else then return None*) 
	|INT x -> Some x
	|FLT n -> None
let to_float :number -> float option = function(*number ->float option*)(*if input is FLT then return, else return None*)
	|FLT  n-> Some n
	|INT  x-> None
let add_number (x:number) (y:number) :number = match (x,y) with(*number->number->number*)(* add two values of type "number"*) 
	|(INT a,FLT b)->FLT ((float a)+.b)(*remember to add those parentheses*)
	|(INT a,INT b)->INT  (a+b)
	|(FLT a,INT b)->FLT  (a+. (float b))
	|(FLT a,FLT b)->FLT  (a+.b)
let sub_number (x:number) (y:number) :number = match (x,y) with(*number->number->number*)(* subtract two values of type "number"*)
        |(INT a,FLT b)->FLT ((float a)-.b)(*remember to add those parentheses*)
        |(INT a,INT b)->INT  (a-b)
        |(FLT a,INT b)->FLT  (a-. (float b))
        |(FLT a,FLT b)->FLT  (a-.b)
let mul_number (x:number) (y:number) :number = match (x,y) with(*number->number->number*)(* multiply two values of type "number"*)
        |(INT a,FLT b)->FLT ((float a)*.b)(*remember to add those parentheses*)
        |(INT a,INT b)->INT  (a*b)
        |(FLT a,INT b)->FLT  (a*. (float b))
        |(FLT a,FLT b)->FLT  (a*.b)
let div_number  (x:number) (y:number) :number = match (x,y) with(*number->number->number*)(* divide two values of type "number"*)
        |(INT a,FLT b)-> FLT ((float a)/.b)(*remember to add those parentheses*)
        |(INT a,INT b)-> if (a mod b) = 0 then INT (a/b) else FLT ((float a)/.(float b))
        |(FLT a,INT b)-> FLT  (a/. (float b))
        |(FLT a,FLT b)-> FLT ( a/.b)


