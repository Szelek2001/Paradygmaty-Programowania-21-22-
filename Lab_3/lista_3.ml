(*Rafal Kruszyna*)

(*Zadanie 2*)
let curry3_zl f x y z = f(x, y, z);;
let curry3_bl = function f -> function x -> function y -> function z -> f(x, y, z);;

let uncurry3_zl f(x, y, z) = f x y z;;
let uncurry3_bl = function f -> function (x, y, z) -> f x y z
(*Zadanie 3*)

let sumProd xs =
    List.fold_left (fun (s, p) h -> (s + h, p * h)) (0, 1) xs;;

sumProd[2; 3; 4; 5] = (14, 120);;
sumProd[-2; 7; 0] = (5, 0);;
sumProd[-5; 3; -4; -9; 10] = (-5, -5400);;

(*Zadanie 5*)

let insertionsort func list =
  let rec insert number list =
    match list with
    | [] -> [number]
    | h :: t when func number h -> h :: insert number t
    | h :: t -> number :: h :: t
  in List.fold_right(insert) list ([]);;

insertionsort (fun x y -> x > y) [3; 2; 8; 2; 6; 1; 5; 7] = [1; 2; 2; 3; 5; 6; 7; 8];;
insertionsort (fun x y -> x < y) [3; 2; 8; 2; 6; 1; 5; 7] = [8; 7; 6; 5; 3; 2; 2; 1];;
insertionsort (fun x y -> x > y) [1; 2; 3; 4] = [1; 2; 3; 4];;





