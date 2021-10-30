(*Rafal Kruszyna*)

(*Zadanie 2*)
let curry3_bl f x y z = f(x, y, z);;
let curry3_zl = function f -> function x -> function y -> function z -> f(x, y, z);;

(*Zadanie 3*)

let sumProd xs =
    List.fold_left (fun (s, p) h -> (s + h, p * h)) (0, 1) xs;;
sumProd [2;3;4];;

