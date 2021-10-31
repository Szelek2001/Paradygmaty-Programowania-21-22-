(*Rafal Kruszyna*)

(*Zadanie 2*)

let f x = f x;;

(*Zadanie 3*)

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let t1 = Node(1,Node(2, Node(4,Empty,Empty),Empty),Node(3,Node(5,Empty,Node(6,Empty,Empty)),Empty));;
let t2 = Node(2,Node(1,Empty,Empty),Node(3,Node(8,Empty,Node(7,Empty,Empty)),Empty))


let breadthBT tree =
let rec breadthBTrec tree =
  match tree with
  | [] -> []
  |Empty::tail -> breadthBTrec tail
  | Node(v, l, r)::tail  -> v::breadthBTrec (l::r::tail)
  in breadthBTrec[tree];;

breadthBT t1 = [1; 2; 4; 3; 5; 6];;
breadthBT t2 = [2; 1; 3; 8; 7];;

type 'a graph = Graph of ('a -> 'a list);;
 let g = Graph
 (function
 0 -> [3]
 | 1 -> [0;2;4]
 | 2 -> [1]
 | 3 -> []
 | 4 -> [0;2]
 | n -> failwith ("Graph g: node "^string_of_int n^" doesn't exist")
);;

