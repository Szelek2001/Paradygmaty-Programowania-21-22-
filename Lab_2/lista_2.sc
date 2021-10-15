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
  def fibIn(n: Int, p: Int, s: Int): Int = n match {
    case 0 => p
    case 1 => s
    case _ => fibIn(n - 1, s, p+s)}
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