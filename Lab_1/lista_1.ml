(*Rafał Kruszyna*)

(*Zadanie 1*)
let rec flatten1 xss =
    if List.length  xss = 0 then []
     else List.hd xss @ flatten1(List.tl xss);;

flatten1 [[1;2]; [1;2]; [63;74]] = [1;2;1;2;63;74];;
flatten1 [["JANEK"; "KOT"]; ["MILOSZ"]] = ["JANEK";"KOT";"MILOSZ"];;
flatten1 [] = [];;


(*Zadanie 2*)

let rec count(x,xs) =
    if List.length  xs = 0 then 0
    else if x=List.hd xs then 1 + count(x,List.tl xs)
    else 0 + count(x,List.tl xs);;

count ("x",["x";"x";"x";"x"])=4;;
count (2,[3;2;1;1])=1;;
count ("y",["2";"3";"4";"fd"])=0;;