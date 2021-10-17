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

(*Zadanie 4*)

let [_; _; xa; _; _;] =  [-2; -1; 0; 1; 2];;
let [(_,_); (xb,_)] = [(1,2);(0,1)]

(*Zadanie 5*)

let rec initSegment (list1,list2) =
    match (list1,list2) with
    | ([],_) -> true
    | (_,[]) -> false
    | (_,_) -> if List.hd list1 = List.hd list2 then initSegment(List.tl list1, List.tl list2) else false;;

initSegment([1;2;3],[1;2;3;4]);;
initSegment([1;2;3],[1;2;4;4]);;

(*Zadanie 6*)

let rec replaceNth (list, n, x) =
    match (list,n) with
    | ([],_) -> []
    | (_,0) -> x :: List.tl list
    | (_,_) -> List.hd list :: replaceNth(List.tl list, n - 1, x);;

replaceNth(["x";"y"], 1,"X");;