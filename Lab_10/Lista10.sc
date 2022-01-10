import scala.collection.mutable
import scala.collection.mutable.Seq


class EmptyQueueException(msg: String) extends Exception(msg)
class CovariantQueue[+T] private (private val q: (List[T], List[T])) {

  def enqueue[S>: T](elem: S): CovariantQueue[S] =
    q match
      case (Nil, _) => new CovariantQueue[S]((List(elem), Nil))
      case (l1, l2) => new CovariantQueue[S](l1, elem :: l2)

  def dequeue: CovariantQueue[T] =
    q match
      case (Nil, _) =>new CovariantQueue[T](Nil,Nil)
      case (_ :: Nil, listEnd) => new CovariantQueue[T](listEnd.reverse, Nil)
      case (_ :: t, listEnd) => new CovariantQueue[T](t, listEnd)

  def first =
    q match
      case (Nil,_) => throw new EmptyQueueException("empty queue")
      case (h :: _, _) => h

  def isEmpty =
    q._1 == Nil
}

object CovariantQueue:
  def empty[T] = new CovariantQueue[T](Nil,Nil)


object TestQueue:
  def main(args: Array[String]): Unit =
    val x1 = CovariantQueue.empty
    println(x1.isEmpty)
    println(x1.enqueue("xd").enqueue("fg").first)
    println(x1.enqueue("xd").enqueue("fg").dequeue.first)




object TestCopy:
  def copy[T](dest: Seq [T], src: Seq[T]) =
    require(dest.length >= src.length)
    var i = 0
    src.foreach(  element =>{
      dest.update(i, element)
      i += 1
    })

  def main(args: Array[String]): Unit =
    var src1 = Seq("xd", "xdd", "xddd")
    var dest1 = Seq("a", "b", "c")

    copy(dest1, src1)

    println(dest1)
