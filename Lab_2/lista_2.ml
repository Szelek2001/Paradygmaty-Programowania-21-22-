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

(*fib(42) = 267914296;;*)
(*fibTail(42) = 267914296;;*)
fib(0) = 0;;
fibTail(0) = 0;;
fib(1) = 1;;
fibTail(1) = 1;;
fib(10) = 55;;
fibTail(10) = 55;;

(*Zadanie 4*)

let [_; _; xa; _; _;] =  [-2; -1; 0; 1; 2];;
let [(_,_); (xb,_)] = [(1,2);(0,1)]

(*Zadanie 5*)

let rec initSegment (list1,list2) =
    match (list1,list2) with
    | ([],_) -> true
    | (_,[]) -> false
    | (_,_) -> if List.hd list1 = List.hd list2 then initSegment(List.tl list1, List.tl list2) else false;;

initSegment([1; 2; 3], [1; 2; 3; 4]) = true;;
initSegment([1; 2; 3], [1; 2; 4; 4]) = false;;
initSegment([], [1; 2; 43; 4]) = true;;

(*Zadanie 6*)

let rec replaceNth (list, n, x) =
    match (list,n) with
    | ([],_) -> []
    | (head::tail,0) -> x :: tail
    | (head::tai,_) -> head :: replaceNth(List.tl list, n - 1, x);;

replaceNth(['o'; 'l'; 'a'; 'm'; 'a'; 'k'; 'o' ;'t' ;'a'], 1, 's') = ['o'; 's'; 'a'; 'm'; 'a'; 'k'; 'o' ;'t' ;'a'];;
replaceNth(['f'; 'r'; 'i'; 'z'], 3, 's') = ['f'; 'r'; 'i'; 's'];;
replaceNth([2; 1; 3; 3], 3, 6) = [2; 1; 3; 6];;