type 'a stream = Cons of 'a * (unit -> 'a stream)
let head (s: 'a stream) : 'a = match s with
  | Cons (v, _) -> v
let tail (s: 'a stream) : 'a stream = match s with
  | Cons (_, tl) -> tl ()
let rec take (n:int) (s : 'a stream) : ('a list) =
 if n = 0 then []
 else match s with
      | Cons (v, tl) -> v :: take (n-1) (tl ())
let rec squares_from n : int stream = Cons (n*n, fun () -> squares_from (n+1) )
let squares = squares_from 1
let rec drop n str : 'a stream = 
  if n = 0 then str
  else match str with
       | Cons (v, tl) ->  drop (n-1) (tl ())
let rec drop_until (f:'a->bool) (str:'a stream) :'a stream = 
  match str with 
  | Cons(v,tl) -> if not(f v) then drop_until f (tl ()) else Cons(v,tl)
let rec map (f:'a->'b) (str:'a stream) :'b stream =
  match str with 
  | Cons(v,tl) ->Cons( (f v) , fun () ->(map f (tl ())))
let rec from n = Cons ( n, fun () -> from (n+1) )
let nats = from 1
(*let rec squares_again = Cons(1.,fun () -> map (fun x -> (sqrt(x)+.1.)*.(sqrt(x)+.1.)) squares_again)*)
let squares_again = Cons(1, fun () -> map (fun x -> (x+1)*(x+1)) nats)
let sqrt_approximations (v:float):float stream = 
  let rec helper upper lower guess target = 
    if (guess*.guess) > target 
    then  Cons(guess, fun () -> helper guess lower ((lower+.guess)/.2.0) v)
    else  Cons(guess, fun () -> helper upper guess ((guess+.upper)/.2.0) v)
  in helper v 1.0 ((1.0+.v)/.2.0) v
let rec diminishing_from n = Cons(n, fun () -> diminishing_from (n/.2.))
let diminishing = diminishing_from 16.
let rec epsilon_diff (epsilon:float) (str: float stream) : float = 
  match str with 
  |Cons(v,tl) -> 
    if v-.head(tl ()) < epsilon
    then head(tl ())
    else epsilon_diff epsilon ( tl () )
let rough_guess = epsilon_diff 1.0 (sqrt_approximations 50.0) 

let precise_calculation = epsilon_diff 0.00001 (sqrt_approximations 50.0) 
let rec sqrt_threshold (v:float) (t:float) : float = 
  let rec helper v t guess = (*use a helper function and apply sqrd_approx to get the next value*)
    match guess with 
    | Cons(n,tl)-> 
      if (Float.abs(n*.n-.v) < t)
      then n
      else helper v t (tl ())
  in helper v t (sqrt_approximations v)
  

