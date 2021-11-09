(*Rafal Kruszyna*)


type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons (k, lazy (lfrom (k+1)));;

let rec toLazyList = function
 [] -> LNil
 | x :: xs -> LCons(x, lazy (toLazyList xs));;

let rec ltake = function
 (0, _) -> []
 | (_, LNil) -> []
 | (n, LCons(x, lazy xs)) -> x :: ltake(n-1, xs);;

let rec toLazyList = function
    [] -> LNil
  | x :: xs -> LCons(x, lazy (toLazyList xs));;

(*Zadanie1*)

let lrepeat k lxs =
  let rec lrepeat2 n lxs =
    match (lxs, n) with
      (LNil, _) -> LNil
    | (LCons(_, lazy tail), 0) -> lrepeat2 k tail
    | (LCons(head, _), _) -> LCons(head, lazy (lrepeat2 (n-1) lxs))
  in lrepeat2 k lxs;;

ltake (12, lrepeat 3 (toLazyList ['a'; 'b'; 'c'; 'd']));;
ltake (15, lrepeat 3 (lfrom 1));;
ltake (15, lrepeat 3 (toLazyList []));;