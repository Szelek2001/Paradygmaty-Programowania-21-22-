import java.util.concurrent.ArrayBlockingQueue
import scala.concurrent.Future

import concurrent.ExecutionContext.Implicits.global


//Rafał Kruszyna

//Zadanie 2

def pairFutZip[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
  fut1.zip(fut2)

object Zad2a:
  def main(args: Array[String]): Unit =
    val arthFut1: Future[Int] = Future {5}
    val arthFut2: Future[Double] = Future {3.0}

    val result1 = pairFutZip(arthFut1, arthFut2)
    println(result1)


def pairFutFor[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
  for
    x <- fut1
    y <- fut2
  yield (x, y)

object Zad2b:
  def main(args: Array[String]): Unit =
    val arthFut1: Future[Int] = Future {6}
    val arthFut2: Future[Double] = Future {7.0}


    val result1 = pairFutFor(arthFut1, arthFut2)
    println(result1)
//GROMNICA
