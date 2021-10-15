//Rafał Kruszyna

//Zadanie 1
def flatten1[A](xss: List[List[A]]): List[A] = {

  if (xss == Nil) Nil
  else xss.head ::: flatten1(xss.tail)
}
flatten1(List(List(2, 3),List(3, 5))) == List(2, 3, 3, 5)
flatten1(List(List("kot", "pchla"),List("conda", "mamamija"))) == List("kot", "pchla", "conda", "mamamija")
flatten1(List(Nil)) == Nil

//Zadanie 2
def count[A](x: A, xs: List[A]): Int = {

  if (xs != Nil) {
    if (x == xs.head) 1 + count(x, xs.tail) else 0 + count(x, xs.tail)
  }
  else 0}

count("x", List("x", "x", "x", "x")) == 4
count(2, List(3, 2, 1, 1)) == 1
count("y",List("2", "3", "4", "fd")) == 0


//Zadanie 3
def replicate[A](x: A, n: Int): List[A] = {
  if (n < 0) throw new Exception("ujemna ilosc powtorzen")
  if (n > 0)
    List(x) ::: replicate(x, n - 1)
  else Nil
}

replicate("le", 3) == List("le", "le", "le")
replicate("kok", 0) == Nil
replicate(4, 2) == List(4, 4)
replicate(6, -2)


//Zadanie 4
def sqrList(xs: List[Int]): List[Int] = {
  if (xs != Nil) {
    List(xs.head * xs.head) ::: sqrList(xs.tail)
  } else Nil
}

//Zadanie 4 Funkcja

val sqrList2: List[Int]=>List[Int]=xs=>
  if xs == Nil then Nil
  else xs.head * xs.head :: sqrList2(xs.tail)

sqrList(List(1, 3, -9)) == List(1, 9, 81)
sqrList(List(0, 9, 3)) == List(0, 81, 9)
sqrList(Nil) == Nil

sqrList2(List(1, 3, -9)) == List(1, 9, 81)
sqrList2(List(0, 9, 3)) == List(0, 81, 9)
sqrList2(Nil) == Nil


//Zadanie 5
def palindrome[A](xs: List[A]): Boolean = {
  xs == xs.reverse
}

palindrome(List("k", "i", "1")) == false
palindrome(List("k", "a", "j", "a", "k")) == true
palindrome(List(2, 3, 2, 3, 2)) == true



//Zadanie 6
def listLength[A](xs: List[A]): Int = {
  if (xs != Nil) {
    1 + listLength(xs.tail)
  }
  else 0
}

listLength(List(1, 3, 2)) == 3
listLength(List("msms", "qwqwqw", "pool", "asdopkasdo")) == 4
listLength(List()) == 0
