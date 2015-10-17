(*This is the module file*)

open Core.Std

module type Arithmetic = sig
    type t
    val to_string : t -> string
    val mul : t -> t -> t
    val add : t -> t -> t
    val mul_identity : t 
    val add_identity : t 
  end

module type Arithmetic_intf = sig
   type t
   type elemType
   val create :   int ->  elemType -> t
   val from_list :  elemType list -> t
   val to_list : t ->  elemType list
   val scalar_add :  elemType -> t -> t 
   val scalar_mul :  elemType -> t -> t
   val scalar_prod : t -> t -> elemType option
   val to_string : t -> string
   val size : t -> int
end

module Make_vector(Vector_elem : Arithmetic): (Arithmetic_intf with type elemType = Vector_elem.t) = struct
                                       (*use = rather than := to not override the type in Arithmetic_intf*)
 type t = | Vector of int * (Vector_elem.t list)
          
 type elemType = Vector_elem.t
 let create size initial =
   let rec list_cre size initial already= 
     if size > 0 then list_cre (size-1) initial (initial::already)
     else already
     in 
     Vector(size, list_cre size initial [])	
    
 let from_list lst = 
   Vector( List.length lst, lst)
 let to_list vec = 
   match vec with
   |Vector(_,lst) -> lst
 let scalar_add v vec = 
   match vec with
   |Vector (size,lst) -> Vector (size, List.map lst (Vector_elem.add v))

 let scalar_mul v vec = 
   match vec with
   |Vector (size, lst)-> Vector (size, List.map lst (Vector_elem.mul v))

 let scalar_prod vec1 vec2 = 
   let rec dot vec1 vec2 acc = 
     match (vec1,vec2) with
     | ([],[]) -> Some acc
     |(v1::lst1),(v2::lst2) -> dot lst1 lst2 (Vector_elem.add acc (Vector_elem.mul v1 v2))
     | _ -> None(*if size of vec1 and vec2 does not match, then return None*)
   in
   match vec1,vec2 with
   |Vector(_,lst1),Vector(_,lst2) -> dot lst1 lst2 Vector_elem.add_identity
   
 let to_string vec = 
   match vec with
   | Vector(size, lst) -> 
       "<< " ^ Int.to_string size ^ " | " ^ 
       (match lst with | [] -> "" 
                       | x::xs -> Vector_elem.to_string x ^ String.concat (List.map xs (fun x-> ", "^ (Vector_elem.to_string x))) 
        )^ " >>"
 let size vec = 
   match vec with 
   | Vector (num,_) -> num
 
end

#use "int_compl_Arith.ml"
