// Rafał Kruszyna

import scala.annotation.tailrec

//Zadanie 2
def fib(n:Int):Int =
  n match {
    case 0 => 0
    case 1 => 1
    case _ => fib(n-2) + fib(n-1)
  }

def fibTail(n: Int): Int ={
  @tailrec
  def fibIn(n: Int, f1: Int, f2: Int): Int = n match {
    case 0 => f1
    case 1 => f2
    case _ => fibIn(n - 1, f2, f1+f2)}
    fibIn(n, 0, 1)}

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
 // Check correct
val List(_, _, xa, _, _)  = List(-2,-1,0,1,2)
val List(_, (xb, _))  = List( (1,2), (0,1) )

//Zadanie 5
def initSegment[A](xs: List[A], ys: List[A]): Boolean =
  (xs,ys) match
    case (Nil,_) => true
    case (_ , Nil) => false
    case (xs,xy) => if xs.head == xy.head then initSegment(xs.tail,ys.tail) else false


initSegment(List(1,2,3),List(1,2,3,4,5))
initSegment(List(1,2,3),List(1,2,4,4,5))

//Zadanie 6

def replaceNth[A](xs: List[A], n: Int, x: A): List[A] =
  (xs,n) match {
    case(Nil,_) => Nil
    case(_,0) => x :: xs.tail
    case(_,_) => xs.head :: replaceNth(xs.tail, n-1,x)
  }

replaceNth(List('o','l','a', 'm', 'a', 'k', 'o', 't', 'a'), 1, 's')