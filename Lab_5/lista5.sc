//Rafal Kruszyna

//Zadanie 1

def lrepeat[A](k: Int)(lxs: LazyList[A]): LazyList[A] =
  def lrepeat2[AA](n: Int)(lxs: LazyList[AA]): LazyList[AA] =
    (lxs, n) match
      case (LazyList(), _) => LazyList()
      case (head #:: tail, 0) => lrepeat2(k)(tail)
      case (head #:: tail, _) => head #:: lrepeat2(n - 1)(lxs)

  lrepeat2(k)(lxs)

lrepeat(3)(LazyList('a', 'b', 'c', 'd')).toList == List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd')
lrepeat(3)(LazyList.from(1)).take(15).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5)
lrepeat(3)(LazyList()).take(15).toList == List()

//Zadanie 2

val lfib: LazyList[Int] =
  def fibonacci(a: Int, b: Int): LazyList[Int] =
    a #:: fibonacci(b, a + b)

  fibonacci(0, 1)

lfib.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
lfib.take(0).toList == List()

//Zadanie 3

//a

sealed trait lBT[+A]

case object LEmpty extends lBT[Nothing]

case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]

def lBreadth[A](ltree: lBT[A]): LazyList[A] =
  def lBreadthrec[A](queue: List[lBT[A]]): LazyList[A] =
    queue match
      case Nil => LazyList()
      case LEmpty :: tail => lBreadthrec(tail)
      case LNode(value, l, r) :: t => value #:: lBreadthrec(t ::: List(l(), r()))

  lBreadthrec(List(ltree))

//b
def lTree(n: Int): lBT[Int] =
  LNode(n, () => lTree(2 * n), () => lTree(2 * n + 1))

lBreadth(lTree(1)).take(20).toList == List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
lBreadth(LEmpty).take(20).toList == List()
