// Rafał Kruszyna

import scala.annotation.tailrec

//Zadanie 2
def fib(n:Int):Int =
  n match {
    case 0 => 0
    case 1 => 1
    case _ => fib(n-2) + fib(n-1)
  }

def fibTail(n: Int): Int =
  @tailrec
  def fibIn(n: Int, f1: Int, f2: Int): Int = n match {
    case 0 => f1
    case 1 => f2
    case _ => fibIn(n - 1, f2, f1+f2)}
    fibIn(n, 0, 1)

fib(42) == 267914296
fibTail(42) == 267914296
fib(0) == 0
fibTail(0) == 0
fib(1) == 1
fibTail(1) == 1
fib(10) == 55
fibTail(10) == 55


//Zadanie 3

//def root3(a: Double):Double = {
//  @tailrec
//  def root3In(a:Double, n:Int):Double = n match {
//    case 0 => a/3
//    case 1 => a
//    case _ => root3In(a,)
//
//
//}}

//Zadanie 4
val List(_, _, xa, _, _)  = List(-2,-1,0,1,2)
val List(_, (xb, _))  = List( (1,2), (0,1) )

//Zadanie 5
def initSegment[A](list1: List[A], list2: List[A]): Boolean =
  (list1,list2) match
    case (Nil,_) => true
    case (_ , Nil) => false
    case (xs,xy) => if xs.head == xy.head then initSegment(xs.tail,list2.tail) else false


initSegment(List(1,2,3),List(1,2,3,4,5)) == true
initSegment(List(1,2,3),List(1,2,4,4,5)) == false
initSegment(List(),List(1)) == true
//Zadanie 6

def replaceNth[A](list: List[A], n: Int, x: A): List[A] =
  (list,n) match {
    case (Nil, _) => Nil
    case (head :: tail, 0) => x :: tail
    case (head :: tail, _) => head :: replaceNth(tail, n-1, x)

  }

replaceNth(List('o','l','a', 'm', 'a', 'k', 'o', 't', 'a'), 1, 's') == List('o', 's', 'a', 'm', 'a', 'k', 'o', 't', 'a')
replaceNth(List('f', 'r', 'i', 'z'), 3, 's') == List('f', 'r', 'i', 's')
replaceNth(List(2, 1, 3, 3), 3, 6) == List(2, 1, 3, 6)