let circle_area_v1 diameter = diameter/.2.0 *. diameter/.2.0 *. 3.1415
let circle_area_v2 diameter = let pi = 3.1415 in diameter/.2.0 *. diameter/.2.0 *. pi
let rec product l = match l with
        | [] -> 1
        | hd :: tl -> hd * product tl
let rec sum_diffs l = match l with
        | [] -> 0
        | x1::[] -> 0
        | x1::(x2::[]) -> x1 - x2
        | x1::x2::tl -> (x1-x2) + sum_diffs (x2::tl)

let distance (x1,y1) (x2,y2) = sqrt((x1-.x2)*.(x1-.x2)+.(y1-.y2)*.(y1-.y2))
let triangle_perimeter (x1,y1) (x2,y2) (x3,y3) = distance (x1,y1) (x2,y2) +. distance (x2,y2) (x3,y3) +. distance (x1,y1) (x3,y3)
let find_head l = match l with
        | hd::tl -> hd
        | [] -> raise (Failure "oops")
let rec perimeter l =  let first = find_head l in
        let rec helper l first =
        match l with
        | x1::x2::[] -> distance x1 x2 +. distance first x2
        | x1::x2::tl ->  distance x1 x2 +. helper (x2::tl) first
        in helper l first
