(*Rafał Kruszyna*)

(*Zadanie 1*)
let rec flatten1 xss =
    if List.length  xss = 0 then []
     else List.hd xss @ flatten1(List.tl xss);;

flatten1 [[1;2]; [1;2]; [63;74]] = [1;2;1;2;63;74];;
 flatten1 [["JANEK"; "KOT"]; ["MILOSZ"]] = ["JANEK";"KOT";"MILOSZ"];;
flatten1 [] = [];;
