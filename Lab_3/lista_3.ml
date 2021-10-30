(*Rafal Kruszyna*)

(*Zadanie 2*)
let curry3_bl f x y z = f(x, y, z);;
let curry3_zl = function f -> function x -> function y -> function z -> f(x, y, z);;

(*Zadanie 3*)

let sumProd xs =
    List.fold_left (fun (s, p) h -> (s + h, p * h)) (0, 1) xs;;

sumProd[2; 3; 4; 5] = (14, 120);;
sumProd[-2; 7; 0] = (5, 0);;
sumProd[-5; 3; -4; -9; 10] = (-5, -5400);;



