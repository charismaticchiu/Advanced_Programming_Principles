let take (n:int) (l:'a list) :('a list)= 
  let rec help num left lst =
    match num with
    | 0 -> lst
    | _ -> (match left with 
	    | [] -> lst
	    | x::[] -> lst @ [x]
	    | x::xs -> (help (num-1) xs (lst @[x])))
  in help n l []

let drop (n:int) (l:'a list) :('a list)= 
  let rec help num left =
    match num with 
    | 0 -> left 
    | _ -> (match left with
            | [] -> []
            | x::[] -> []
            | x::xs -> (help (num-1) xs))
  in help n l

let take_while (f: 'a -> bool) (l: 'a list): 'a list =
  let rec help fct lst newL = 
    match lst with 
    | [] -> []
    | x::[] -> (match (fct x) with 
 		| true -> newL@[x]
		| _ -> newL)
    | x::xs -> (match (fct x) with
                | true -> help fct xs (newL@[x])
                | _ -> help fct xs (newL))
  in help f l []


type estring = char list

let string_to_estring s = String.to_list s

let estring_to_string es = String.concat (List.map es Char.to_string )

let capitalize (l:estring) :estring =
  List.map l (fun x -> Char.uppercase(x) )  
