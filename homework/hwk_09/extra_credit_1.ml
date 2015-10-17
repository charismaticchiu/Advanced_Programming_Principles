exception KeepLooking
exception Case_doesnt_exist
let show_int_int_pair (x,y) = "(" ^ (Int.to_string x) ^ "," ^ (Int.to_string y) ^ ")"
let rec foldr f v l = match l with
  | [] -> v
  | x::xs -> f x (foldr f v xs)
let is_elem v l =
  foldr (fun x in_rest -> if x = v then true else in_rest) false l
let show_list show l =
  let rec sl l =
    match l with
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl l ^ " ]"
let maze_move (pos:int*int) : (int*int) list = (*never go back to the previous step*)
  match pos with
  (*|(1,1) -> [(2,1)]
  |(2,1) -> [(3,1)]*)
  |(3,1) -> [3,1](*keeplooking case*)
  (*|(4,1) -> [(4,2)]*)
  |(5,1) -> [5,1](*raise Found*)
  |(1,2) -> [(2,2)]
  |(2,2) -> [(3,2)]
  |(3,2) -> [(3,3);(3,1);(4,2)]
  |(4,2) -> [4,2](*raise KeepLooking(*[(4,1);(3,2)]*)*)
  |(5,2) -> [(5,1)]
  |(1,3) -> [(1,2);(1,4)](*eliminate (2,3); silly to go back*)
  |(2,3) -> [(1,3)]
  |(3,3) -> [(4,3);(3,4)]
  |(4,3) -> [(5,3)]
  |(5,3) -> [(5,2);(5,4)]
  |(1,4) -> [(1,5)]
  |(2,4) -> [(3,4)]
  |(3,4) -> [(4,4);(3,3)]
  |(4,4) -> [(4,5)]
  |(5,4) -> [5,4](*raise KeepLooking(*[(5,3)]*)*)
  |(1,5) -> [(2,5)]
  |(2,5) -> [(2,4)]
  |(3,5) -> [3,5]
  |(4,5) -> [(3,5);(5,5)]
  |(5,5) -> [(5,5)](*keeplooking case*)
  |(_,_) -> raise Case_doesnt_exist

let rec process_solution_maze (s:(int*int) list): 'a option =
  print_endline ("Here is a solution: " ^ show_list show_int_int_pair s) ;
  print_endline ("Do you like it ?" ) ;
  match In_channel.input_line stdin with
  | None -> (* user typed ^D *)
     print_endline "Please enter a response.";
     process_solution_maze s     
  | Some answer ->
     if is_elem 'n' (String.to_list answer)
     then raise KeepLooking(*if not satisfied with the answer, then keep looking*)
     else (print_endline "Thanks for playing..." ; Some s)
let rec maze_v2 ( f : (int * int) list -> 'a option) : 'a option =
  let rec maze_run2 lst acc =
    match lst with
    |[3,5] -> f acc
    |[5,1] -> f  (acc @ lst)
    |[3,1]|[4,2]|[5,4]|[5,5] -> raise KeepLooking(*if dead end, then Keep Looking*)
    | [] -> raise KeepLooking (*similar to deadend*)
    |x::[] -> if is_elem x acc
              then raise KeepLooking(*have to see if the path have been moved*)
              else maze_run2 (maze_move x) (acc @ [x]) 
    |x::xs -> (try maze_run2 (maze_move x) (acc @ [x]) (*if exception then run the rest*)
	       with KeepLooking -> maze_run2 xs acc )
  in try maze_run2 (maze_move (2,3)) [(2,3)] with KeepLooking -> None(*start with (2,3)*)

let maze_first () = maze_v2 (fun x -> Some x)
let maze_interactive () =
  maze_v2 process_solution_maze
let maze_print_all () =
   maze_v2
    (fun x -> print_endline (show_list show_int_int_pair x) ;
          raise KeepLooking)
