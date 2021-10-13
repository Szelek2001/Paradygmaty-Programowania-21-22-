(*Rafał Kruszyna*)

(*Zadanie 1*)
let rec flatten1 xss =
    if List.length  xss = 0 then []
     else List.hd xss @ flatten1(List.tl xss);;

flatten1 [[1; 2]; [1; 2]; [63; 74]] = [1; 2; 1; 2; 63; 74];;
flatten1 [["JANEK"; "KOT"]; ["MILOSZ"]] = ["JANEK"; "KOT"; "MILOSZ"];;
flatten1 [] = [];;


(*Zadanie 2*)

let rec count(x, xs) =
    if List.length  xs = 0 then 0
    else if x=List.hd xs then 1 + count(x, List.tl xs)
    else 0 + count(x, List.tl xs);;

count ("x", ["x"; "x"; "x"; "x"]) = 4;;
count (2,[3;2;1;1])=1;;
count ("y", ["2"; "3"; "4"; "fd"]) = 0;;

(*Zadanie 3*)

let rec replicate(x, n) =
    if n < 0 then failwith "ujemna ilosc powtorzen"
    else if(n > 0) then [x] @ replicate(x, n-1)
    else [];;

replicate ("x", 3) = ["x"; "x"; "x"];;
replicate (3, 4) = [3; 3; 3; 3];;
replicate ("mama", 0) = [];;
(*Zadanie 4*)

let rec sqrList xs =
    if List.length  xs = 0 then []
else [List.hd xs * List.hd xs] @ sqrList(List.tl xs);;

sqrList [3; 4; 5]=[9; 16; 25];;
sqrList [1; 0; 3]=[1; 0; 9];;

(*Zadanie 5*)

let  palindrome(xs) = (
    xs= List.rev xs
);;

palindrome ["xd"; "xd"] = true;;
palindrome [1; 2; 1] = true;;
palindrome ["xd"; "kek"] = false;;

(*Zadanie 6*)

let rec listLength xs =
    if  xs = [] then 0
    else 1 + listLength(List.tl xs);;

listLength ["maslo"; "kawa"; "mleko"] = 3;;
listLength [3; 2; 4; 5] = 4;;
listLength [] = 0;;