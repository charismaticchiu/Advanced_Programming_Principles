let rec filter (f:'a -> bool) (l:'a list) : 'a list = match l with
  | [] -> []
  | x::xs -> let rest = filter f xs
	     in if f x then x :: rest else rest
let rec foldr f v l = match l with
  | [] -> v
  | x::xs -> f x (foldr f v xs)
let rec foldl f accum l = match l with 
  | [ ] -> accum
  | x::xs -> foldl f (f accum x) xs
let rec map f l = match l with
  | [ ] -> [ ]
  | x::xs -> f x :: map f xs
let take (n:int) (l:'a list) :('a list)=
  let rec help num left lst = match num with
    | 0 -> lst
    | _ -> (match left with
            | [] -> lst
            | x::xs -> (help (num-1) xs (lst @[x])))
  in help n l []
let drop (n:int) (l:'a list) :('a list)=
  let rec help num left = match num with
    | 0 -> left
    | _ -> (match left with
            | [] -> []
            | x::xs -> (help (num-1) xs))
  in help n l
(*if var=any one of the element in the list then true*)
let is_elem (v: 'a) (l: 'a list) : bool =
  let fct (var,have) lst = if (var=lst)||have then (var,true) else (var, false)
  in
  let (_,have) = foldl fct (v,false) l in have
let length (l:'a list): int=
  let fct (count) lst = if [lst]=[] then count else (count+1)
  in
  foldl fct 0 l
let fct1 ((count:int),(sep:'a list),(spt:int list),l) lst =
    if (is_elem lst sep)
    then ((count+1),sep,spt@[count],l) else ((count+1),sep,spt,l)
  (*spt is seprating points; is elem is sep then start over*)(*sptd:seperated*)
let fct2 spt (lst,sptd) = if sptd=[[]]
                            then ((take (spt-1) lst),[(drop (spt) lst)])(*attention in the first*)
                            else ((take (spt-1) lst),(drop (spt) lst)::sptd)
let split_by (eq:'a->'a->bool) (l:'a list) (sep:'a list)=
  let (_,_,spts,l) = foldl fct1 (1,sep,[],l) l (*start from 1*)
  in let (left,sptd) = foldr fct2 (l,[[]]) spts
  in if sptd=[[]] 
     then [(take (length(left)) left)] 
     else (take (length(left)) left)::sptd
type word = char list
type line = word list
let convert_to_non_blank_lines_of_words (str:string): 'a list= 
  let fctB (eleB) lst = match lst with | [] -> eleB | _ -> if eleB =[[]] then [lst] else eleB@[lst]
  in (*eliminate '\n'*)
  let fctW (eleS) lst = match lst with | a-> if eleS = [[[]]] then [(split_by (=) a [' '])] else eleS@[(split_by (=) a [' '])]
  in (*to_words*)
  let fctS eleS lst :char list list list= match lst with | [[]]->eleS | _ ->if eleS =[[[]]] then [lst] else eleS@[lst]
  in (*eliminate ' 'space*)
  let charl = String.to_list str
  in let splittedB = split_by (=) charl ['\n']
  in let lines = foldl fctB ([[]]) splittedB
  in let words = foldl fctW ([[[]]]) lines 
  in let wordsE = foldl fctS ([[[]]]) words
  in let eleE = match wordsE with 
     |[[[]]] -> []
     | _ -> wordsE
  in eleE(*let splittedS = split_by (=) eleE [' '] in splittedS*)
let text = In_channel.read_all "not_a_paradelle_wrong_line_count.txt" in length(convert_to_non_blank_lines_of_words text)

assert (is_elem 4 [1;2;3;4;5;6])
assert (not (is_elem 7 [1;2;3;4;5;6;8;9;10] ) )
assert (is_elem "Hello" ["Why"; "not";  "say"; "Hello"])
assert (not (is_elem 3.5 [ ]) )


assert ( split_by (=) [1;2;3;4;5;6;7;8;9;10;11] [3;7] =
         [ [1;2]; [4;5;6]; [8;9;10;11] ] )
assert ( split_by (=) ["A"; "B"; "C"; "D"] [] =
         [["A"; "B"; "C"; "D"]] )


assert ( length [] = 0 )
assert ( length [1;2;3;4] = 4 )
assert ( length ["Hello"] = 1 )


assert ( let text = In_channel.read_all "paradelle_susan_1.txt"
         in length (convert_to_non_blank_lines_of_words text) = 24 )

assert ( let text = In_channel.read_all "paradelle_susan_2.txt"
         in length (convert_to_non_blank_lines_of_words text) = 24 )

assert ( let text = In_channel.read_all "paradelle_emma_1.txt"
         in length (convert_to_non_blank_lines_of_words text) = 24 )

assert ( let text = In_channel.read_all "not_a_paradelle_susan_1.txt"
         in length (convert_to_non_blank_lines_of_words text) = 24 )

assert ( let text = In_channel.read_all "not_a_paradelle_susan_2.txt"
         in length (convert_to_non_blank_lines_of_words text) = 24 )

assert ( let text = In_channel.read_all "not_a_paradelle_emma_1.txt"
         in length (convert_to_non_blank_lines_of_words text) = 24 )

assert ( let text = In_channel.read_all "not_a_paradelle_empty_file.txt"
         in length (convert_to_non_blank_lines_of_words text) = 0 )

assert ( let text = In_channel.read_all "not_a_paradelle_wrong_line_count.txt"
         in length (convert_to_non_blank_lines_of_words text) = 9 )

assert ( let text = In_channel.read_all "paradelle_susan_1.txt"
     in  match convert_to_non_blank_lines_of_words text with
         | line1::rest -> length line1 = 9
         | _ -> false ) 
