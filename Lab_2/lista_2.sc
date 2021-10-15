// Rafał Kruszyna

//Zad 2
def f(n:Int):Int =
  n match {
    case 0 => 0
    case 1 => 1
    case _ => f(n-2) + f(n-1)
  }
