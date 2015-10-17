open Core.Std
open Hwk_10

module Int_arithmetic : (Arithmetic with type t = int)= struct
  type t = int
  let to_string  = Int.to_string
  let add_identity = 0
  let mul_identity = 1
  let mul = ( * )
  let add = ( + )
end


module Int_vector = Make_vector (Int_arithmetic)
let v1 = Int_vector.create 10 1
let v2 = Int_vector.from_list [1;2;3;4;5]
let v3 = Int_vector.scalar_add 3 v2

let v4 = Int_vector.scalar_mul 10 v2

let i1 = Int_vector.scalar_prod v3 v4

let l1 = Int_vector.to_list v3 

let i2 = Int_vector.size v4

let s1 = Int_vector.to_string v1

let s2 = Int_vector.to_string v2

let s3 = Int_vector.to_string v3

let s4 = Int_vector.to_string v4

module Complex_arithmetic : (Arithmetic with type t = float * float) = struct
  type t = float * float
  let to_string v = match v with (l,r) -> "("^Float.to_string l ^"+"^Float.to_string r ^"i)"
  let mul v1 v2 = match v1,v2 with
    | (r1,i1),(r2,i2) -> ((r1 *. r2) -. (i1 *. i2), (i1 *. r2) +. (i2 *. r1))
  let add v1 v2 = match v1,v2 with 
    | (r1,i1),(r2,i2) -> (r1 +. r2,i1 +. i2)
  let add_identity = (0.,0.)
  let mul_identity = (1.,0.)
  
end

module Complex_vector = Make_vector (Complex_arithmetic)
let v5 = Complex_vector.from_list [ (1.0, 2.0); (3.0, 4.0); (5.0, 6.0) ]

let v6 = Complex_vector.scalar_add (5.0, 5.0) v5

let c1 = Complex_vector.scalar_prod v5 v6

let s5 = Complex_vector.to_string v5

let s6 = Complex_vector.to_string v6
