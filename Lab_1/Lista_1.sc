//Rafał Kruszyna

//Zadanie 1
def flatten1[A](xss: List[List[A]]): List[A] =
  if xss == Nil then Nil
  else xss.head ::: flatten1(xss.tail)

flatten1(List(List(5, 6), List(1, 2, 3))) == List(5, 6, 1, 2, 3)
flatten1(List(List("kot", "pchla"), List("conda", "mamamija"))) == List("kot", "pchla", "conda", "mamamija")
flatten1(List(Nil)) == Nil

//Zadanie 2
def count[A](x: A, xs: List[A]): Int =
  if xs != Nil then
    if (x == xs.head) 1 + count(x, xs.tail) else 0 + count(x, xs.tail)
  else 0

count('a', List('a', 'l', 'a')) == 2
count(2, List(3, 2, 1, 1)) == 1
count("y", List("2", "3", "4", "fd")) == 0

//Zadanie 3
def replicate[A](x: A, n: Int): List[A] =
  if (n < 0) throw new Exception("ujemna ilosc powtorzen")
  if n > 0 then
    x :: replicate(x, n - 1)
  else Nil

replicate("la", 3) == List("la", "la", "la")
replicate("kok", 0) == Nil
replicate(4, 2) == List(4, 4)

//Zadanie 4 Metoda
def sqrList(xs: List[Int]): List[Int] =
  if xs != Nil then
    List(xs.head * xs.head) ::: sqrList(xs.tail)
  else Nil

sqrList(List(1, 2, 3, -4)) == List(1, 4, 9, 16)
sqrList(List(0, 9, 3)) == List(0, 81, 9)
sqrList(Nil) == Nil

//Zadanie 4 Funkcja

val sqrList2: List[Int] => List[Int] = xs =>
  if xs == Nil then Nil
  else xs.head * xs.head :: sqrList2(xs.tail)

sqrList2(List(1, 2, 3, -4)) == List(1, 4, 9, 16)
sqrList2(List(0, 9, 3)) == List(0, 81, 9)
sqrList2(Nil) == Nil

//Zadanie 5
def palindrome[A](xs: List[A]): Boolean =
  xs == xs.reverse

palindrome(List('a', 'l', 'a')) == true
palindrome(List("k", "a", "j", "a", "k")) == true
palindrome(List(2, 3, 2, 3, 2)) == true

//Zadanie 6
def listLength[A](xs: List[A]): Int =
  if (xs != Nil) then
    1 + listLength(xs.tail)
  else 0

listLength(List(1, 3, 2)) == 3
listLength(List("msms", "qwqwqw", "pool", "asdopkasdo")) == 4
listLength(List()) == 0
