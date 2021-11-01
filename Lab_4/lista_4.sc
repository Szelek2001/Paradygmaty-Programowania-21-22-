// Rafał Kruszyna

//Zadanie 3

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
val t1 = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty))
val t2 = Node(2, Node(1, Empty, Empty), Node(3, Node(8, Empty, Node(7, Empty, Empty)), Empty))

def breadthBT[A](tree: BT[A]) =
  def breadthBTrec[A](tree: List[BT[A]]): List[A] =
    tree match
      case Nil => Nil
      case Empty :: tail => breadthBTrec(tail)
      case Node(v, l, r) :: tail => v :: breadthBTrec(l :: r :: tail)

  breadthBTrec(List(tree))

breadthBT(t1) == List(1, 2, 4, 3, 5, 6)
breadthBT(t2) == List(2, 1, 3, 8, 7)
breadthBT(t) == List(1, 2, 3)

//Zadanie 5

sealed trait Graphs[A]

case class Graph[A](succ: A => List[A]) extends Graphs[A]


def depthSearch[A](g: Graph[A])(startNode: A): List[A] =
  def search(visited: List[A])(queue: List[A]): List[A] =
    queue match
      case Nil => Nil
      case h :: t => if visited contains h then search(visited)(t) else h :: search(h :: visited)((g succ h) ::: t)

  search(Nil)(List(startNode))

val g = Graph((i: Int) => i match
  case 0 => List(3)
  case 1 => List(0, 2, 4)
  case 2 => List(1)
  case 3 => Nil
  case 4 => List(0, 2)
  case n =>
    throw new Exception(s"Graph g: node $n doesn't exist")
)

val g1 = Graph((i: Int) => i match
  case 0 => List(3, 4)
  case 1 => List(0, 2, 4)
  case 2 => Nil
  case 3 => Nil
  case 4 => List(0, 2)
  case n =>
    throw new Exception(s"Graph g: node $n doesn't exist")
)

depthSearch(g)(4) == List(4, 0, 3, 2, 1)
depthSearch(g)(3) == List(3)
depthSearch(g1)(0) == List(0, 3, 4, 2)