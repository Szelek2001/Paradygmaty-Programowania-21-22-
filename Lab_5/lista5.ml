(*Rafal Kruszyna*)

type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;

let rec ltake = function
  |(0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x, lazy xs)) -> x :: ltake(n-1, xs);;

let rec toLazyList = function
  | [] -> LNil
  | x :: xs -> LCons(x, lazy (toLazyList xs));;

(*Zadanie 1*)

let lrepeat k lxs =
  let rec lrepeat2 n lxs =
    match (lxs, n) with
    | (LNil, _) -> LNil
    | (LCons(_, lazy tail), 0) -> lrepeat2 k tail
    | (LCons(head, _), i) -> LCons(head, lazy (lrepeat2 (i-1) lxs))
  in lrepeat2 k lxs;;

ltake (12, lrepeat 3 (toLazyList ['a'; 'b'; 'c'; 'd'])) = ['a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'c'; 'c'; 'c'; 'd'; 'd'; 'd'];;
ltake (15, lrepeat 3 (lfrom 1)) = [1; 1; 1; 2; 2; 2; 3; 3; 3; 4; 4; 4; 5; 5; 5];;
ltake (15, lrepeat 3 (toLazyList [])) = [];;

(*Zadanie 2*)
let lfib =
  let rec fibonacci a b =
    LCons(a, lazy (fibonacci b (a + b)))
  in fibonacci 0 1;;

ltake (10, lfib) = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34];;
ltake (0, lfib) = [];;

(*Zadanie 3*)
type 'a lBT = LEmpty  |  LNode of  'a * (unit ->'a lBT) * (unit -> 'a lBT);;

(*Podpunkt a*)
let lBreadth ltree =
  let rec lBreadthRec queue =
    match queue with
    | [] -> LNil
    | LEmpty :: tail -> lBreadthRec tail
    | LNode(value, l, r) :: tail -> LCons(value, lazy (lBreadthRec (tail @ [l(); r()])))
  in lBreadthRec [ltree];;

(*Podpunkt b*)
let rec lTree n =
  LNode(n, (function() -> lTree (2 * n)), (function() -> lTree (2 * n + 1)));;

ltake(20, lBreadth(lTree 1)) = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20];;
ltake(20, lBreadth(LEmpty)) = [];;
ltake(10, lBreadth(lTree 7)) = [7; 14; 15; 28; 29; 30; 31; 56; 57; 58];;