//Rafał Kruszyna

//Zadanie 2

def curry3_zl[A, B, C, D](f: (A, B, C) => D) =
  (x: A) => (y: B) => (z: C) => f(x, y, z)

def curry3_bl[A, B, C, D](f: (A, B, C) => D)
                         (x: A)(y: B)(z: C): D = f(x, y, z)

def uncurry3_zl[A, B, C, D](f: A => B => C => D) =
  (x: A, y: B, z: C) => f(x)(y)(z)

def uncurry3_bl[A, B, C, D](f: A => B => C => D)
                           (x: A, y: B, z: C): D = f(x)(y)(z)


//Zadanie 3

def sumProd(xs: List[Int]): (Int, Int) = (xs.foldLeft(0, 1)) ((acc: (Int, Int), h: Int) => (acc._1 + h, acc._2 * h))

sumProd(List(2, 3, 4)) == (9, 24)
sumProd(List(0, 1, 0)) == (1, 0)
sumProd(List(-6, 1, 1, 1)) == (-3, -6)

//Zadanie 5

def insertionsort[A](function: (A, A) => Boolean, list: List[A]): List[A] =
  def insert(number: A, list: List[A]): List[A] =
    list match
      case Nil => number :: Nil
      case h :: t if function(number, h) => h :: insert(number, t)
      case h :: t => number :: h :: t

  list.foldRight(List.empty[A])(insert)


insertionsort((a: Int, b: Int) => a > b, List(9, 28, 7, 6, 5, 5, 4, 3, 2, 1)) == List(1, 2, 3, 4, 5, 5, 6, 7, 9, 28)
insertionsort((a: Int, b: Int) => a < b, List(9, 28, 7, 6, 5, 5, 4, 3, 2, 1)) == List(28, 9, 7, 6, 5, 5, 4, 3, 2, 1)
insertionsort((a: Int, b: Int) => a > b, List(1, 2, 3, 4)) == List(1, 2, 3, 4)