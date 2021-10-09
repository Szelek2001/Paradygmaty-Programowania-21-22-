//Rafał Kruszyna

//Zadanie 1
def flatten1[A](xss: List[List[A]]): List[A] = {

  if (xss == Nil) Nil
  else xss.head ::: flatten1(xss.tail)
}
flatten1(List(List(2,3),List(3,5))) == List(2,3,3,5)
flatten1(List(Nil))==Nil

//Zadanie 2
def count[A](x: A, xs: List[A]): Int = {

  if (xs != Nil) {
    if (x == xs.head) 1 + count(x, xs.tail) else 0 + count(x, xs.tail)
  }
  else 0}

count("x",List("x","x","x","x"))==4
count("mama",List("mama","tata","x","x"))==1
count("y",List("2","3","4","fd"))==0


//Zadanie 3
def replicate[A](x: A, n: Int): List[A] = {

  if (n > 0)
    List(x) ::: replicate(x, n - 1)
  else Nil
}

replicate("le",3) == List("le","le","le")
replicate("kok",0) == Nil
replicate(4,2) == List(4,4)

//Zadanie 4
def sqrList(xs: List[Int]): List[Int] = {
  if (xs != Nil) {
    List(xs.head * xs.head) ::: sqrList(xs.tail)
  } else Nil
}

sqrList(List(1,3,-9))== List(1,9,81)
sqrList(List(0,9,3))== List(0,81,9)
sqrList(Nil) == Nil


//Zadanie 5
def palindrome[A](xs: List[A]): Boolean = {
  xs == xs.reverse
}

palindrome(List("k","i","1")) == false
palindrome(List("k","a","j","a","k")) == true


//Zadanie 6
def listLength[A](xs: List[A]): Int = {
  if (xs != Nil) {
    1 + listLength(xs.tail)
  }
  else 0
}

listLength(List("a","v","d"))==3
listLength(List("msms","qwqwqw","pool","asdopkasdo"))==4
listLength(List())==0
