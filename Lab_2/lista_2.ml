(*Rafał Kruszyna*)

(*Zadanie 2*)
let rec fib n =
    match n with
        0 -> 0
       |1 -> 1
       | _ -> fib(n-2) + fib(n-1);;

let fibTail n =
    let rec fibIn(n,f1,f2) =
      match n with
        0 -> f1
       |1 -> f2
       | _ -> fibIn(n-1, f2, f1+f2) in
       fibIn(n, 0, 1);;

(*Zadanie 4*)

let [_; _; x; _; _;] =  [2; 3; 4; 5; 1;];;