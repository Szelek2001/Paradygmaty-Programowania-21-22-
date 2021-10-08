//Rafał Kruszyna

//Zadanie 1
def flatten1[A](xss: List[List[A]]): List[A] = {

  if (xss == Nil) Nil
  else xss.head ++ flatten1(xss.tail)
}
//Zadanie 2
def count[A](x: A, xs: List[A]): Int = {

  if (xs != Nil) {
    val ile = if (x == xs.head) 1 else 0
    ile + count(x, xs.tail)
  }
  else
    0
}

def replicate[A](x: A, n: Int): List[A] = {

  if (n > 0)
    List(x) ++ replicate(x, n - 1)
  else Nil

}

def sqrList(xs: List[Int]): List[Int] = {
  if (xs != Nil) {
    List(xs.head * xs.head) ++ sqrList(xs.tail)
  } else Nil

}

def palindrome[A](xs: List[A]): Boolean = {
  xs == xs.reverse
}
def listLength[A](xs: List[A]): Int = {
  if (xs != Nil) {
    1 + listLength(xs.tail)
  }
  else 0

}