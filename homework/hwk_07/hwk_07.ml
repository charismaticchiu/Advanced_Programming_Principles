type result = OK 
        | FileNotFound of string
        | IncorrectNumLines of int 
        | IncorrectLines of (int * int) list
        | IncorrectLastStanza
type word = char list
type line = word list

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
let is_elem_with (f) (v: 'a) (l: 'a list) : bool =
  let fct (f,var,have) lst = if (f lst var)||have then (f,var,true) else (f,var, false)
  in(*in f lst var, notice the order matters*)
  let (_,_,have) = foldl fct (f,v,false) l in have
let length (l:'a list): int=
  let fct (count) lst = if [lst]=[] then count else (count+1)
  in
  foldl fct 0 l
let fct1 (f,(count:int),(sep:'a list),(spt:int list),l) lst =
    if (is_elem_with f lst sep)
    then (f,(count+1),sep,spt@[count],l) else (f,(count+1),sep,spt,l)
  (*spt is seprating points; is elem is sep then start over*)(*sptd:seperated*)
let fct2 spt (lst,sptd) = if sptd=[[]]
                            then ((take (spt-1) lst),[(drop (spt) lst)])(*attention in the first*)
                            else ((take (spt-1) lst),(drop (spt) lst)::sptd)
let split_by (eq:'a->'a->bool) (l:'a list) (sep:'a list)=
  let (_,_,_,spts,l) = foldl fct1 (eq,1,sep,[],l) l (*start from 1; spts denotes the seperators' position*)
  in let (left,sptd) = foldr fct2 (l,[[]]) spts
  in if sptd=[[]] 
     then [(take (length(left)) left)] 
     else (take (length(left)) left)::sptd
let get_text (fn:string) : string option =
  try
      Some (In_channel.read_all fn)
  with 
  | _ -> None
let ele_punc_lower (str:string) :char list=(*eliminate , . : and replace hyphen with space and change to lowercase*)
  let fctH eleHlst lst = match lst with |'-'|','|'.'|':'|';'|'?'|'!' -> eleHlst@[' '] | _ -> eleHlst@[lst]
  in (*above is to replace hyphen with a space*)
  let fctL lowerL lst = match lst with | v -> lowerL@[Char.lowercase v]
  in
  let charl = String.to_list str
  in let hyphen_eled = foldl fctH ([]) charl
  in foldl fctL ([]) hyphen_eled
let convert_to_non_blank_lines_of_words (str:string): 'a list= 
  let charl = ele_punc_lower str
  in let splittedB = split_by (=) charl ['\n']
  in filter (fun x -> x <> []) (map (fun x -> filter (fun x -> x <> []) (split_by (=) x [' '])) splittedB)
let split_last (l:char list list list)=(*seperate the first three stanzas and the fourth*)
  let fctC (three,last,count) lst = 
    if (count < 18) then (three@lst,last,count+1) else (three,last@lst,count+1)
  in let (three,last,_) = foldl fctC ([],[],0) l in (three,last)
let split_first_three (l:char list list list)=(*to have 1st,2nd,3rd stanza seperately*)
  let split_h (one,two,three,count) lst =
    match count with 
    |1 |2 |3 |4 |5 |6  -> (one@[lst],two,three,count+1)
    |7 |8 |9 |10|11|12 -> (one,two@[lst],three,count+1)
    |13|14|15|16|17|18 -> (one,two,three@[lst],count+1)
    |_ -> (one,two,three,count)
  in let (one,two,three,_) = foldl split_h ([],[],[],1) l 
  in (one,two,three)
let is_iden (e1,e2):bool=(*this function is used to check if two lines are the same; must have same length and same elements*)
  (length(e1)=length(e2))&&((List.dedup e1)=(List.dedup (e1@e2)))
let stanza_check (para:int)(*paragraph num*) (stanza:char list list list):(int*int) list= (*check if there are errors in a stanza*)
  let lastTwoCheck fourLine lastTwo = if length(fourLine)=length(lastTwo) then true else false
  in
  match stanza with
  |l1::l2::l3::l4::l5::l6::[] -> (
    match (is_iden(l1,l2),is_iden(l3,l4),lastTwoCheck (l1@l3) (l5@l6))with
     (*Note:if 1,2 or 3,4are not identical then 5,6 lines don't care*)
    |(true,true,true) ->[]
    |(false,true,true)->[(1+para*6,2+para*6)]
    |(false,false,true)->[(1+para*6,2+para*6);(3+para*6,4+para*6)]
    |(false,false,false)->[(1+para*6,2+para*6);(3+para*6,4+para*6)]
    |(false,true,false)->[(1+para*6,2+para*6);]
    |(true,false,true)->[(3+para*6,4+para*6)]
    |(true,true,false)->[(5+para*6,6+para*6)]
    |(true,false,false)->[(1+para*6,2+para*6)])
(**)
let paradelle (fileName:string) : result =
  let lastStanzaCheck (three,flag) lst = if flag&&(is_elem lst three) then (three,true) else (three,false)
  in 
  match  get_text fileName with
  |None -> FileNotFound(fileName)
  |Some t -> let content = t 
    in let word_line =convert_to_non_blank_lines_of_words content 
    in let num_line = length(word_line)(*first check line numbers*)
    in
    if num_line<>24 then IncorrectNumLines(num_line) 
    else
    let (one,two,three)= split_first_three word_line (**)
    in 
    (match (stanza_check (1-1) one)@(stanza_check (2-1) two)@(stanza_check (3-1) three)with
    |[] ->(*if no errors in first three stanzas*)
      let (firstThree,last) = split_last word_line(*we get first three stanzas as a whole and last stanza as a whole*)
      in 
      (match foldl lastStanzaCheck (firstThree,true) last with(*check if last stanza use every words of the previous stanzas*)
      |(_,false)->IncorrectLastStanza
      |_ ->OK)
    |arr -> IncorrectLines(arr))
(*
let text = In_channel.read_all "not_a_paradelle_susan_2.txt" 
let(s1,s2,s3) = (split_first_three (convert_to_non_blank_lines_of_words text))  
match s1 with
  |a::b::c::d::e::f::[] ->(c,d)*)(* (
    match ((length(a)=length(List.dedup (a@b))),(length(c)=length(List.dedup (c@d))))with
    |(true,false)->(1,0)
    |(true,true)->(1,1)
    |(false,true)->(0,1)
    |(false,false)->(0,0))*)
(*
assert ( paradelle "paradelle_susan_1.txt" = OK )
assert ( paradelle "paradelle_susan_2.txt" = OK )
assert ( paradelle "paradelle_emma_1.txt"  = OK )

assert ( paradelle "not_a_paradelle_susan_1.txt" <> OK )
assert ( paradelle "not_a_paradelle_susan_2.txt" <> OK )
assert ( paradelle "not_a_paradelle_emma_1.txt"  <> OK )

assert ( paradelle "not_a_paradelle_empty_file.txt"  <> OK )
assert ( paradelle "not_a_paradelle_wrong_line_count.txt"  <> OK )


assert ( paradelle "not_a_paradelle_susan_1.txt" = 
       IncorrectLines [(1, 2); (11, 12); (17, 18)] )

assert ( paradelle "not_a_paradelle_susan_2.txt" =
       IncorrectLines [(11, 12); (17, 18)] )

assert ( paradelle "not_a_paradelle_susan_3.txt" = 
       IncorrectLines [(1, 2); (11, 12); (17, 18)] )

assert ( paradelle "not_a_paradelle_emma_1.txt" = 
       IncorrectLastStanza )

assert ( paradelle "not_a_paradelle_empty_file.txt"  =
       IncorrectNumLines 0 ) 

assert ( paradelle "not_a_paradelle_wrong_line_count.txt" =
       IncorrectNumLines 9 )
*)
(*  
  split 4 stanza; first 3 12,34 same, 56 is elem of 1234; 
  last stanza every elem in stanza 123.*)
(*  
let text = In_channel.read_all "paradelle_emma_1.txt" in split_first_three (convert_to_non_blank_lines_of_words text)  
*)
(*
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
*)
