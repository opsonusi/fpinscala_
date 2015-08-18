sealed trait MyList[+A]
case class MyCons[A](x: A, xs: MyList[A]) extends MyList[A]
case object MyNil extends MyList[Nothing]

object MyList {
    def tail[A](l: MyList[A]) = {
        l match {
            case MyCons(_, xs) => xs
            case _ => l
        }
    }

    def apply[A](as: A*): MyList[A] = {
        if(as.isEmpty) MyNil
        else MyCons(as.head, apply(as.tail: _*))
    }
  
  def setHead[A](head: A, l: MyList[A]) = {
    l match {
      case MyCons(_, xs) => MyCons(head, xs)
      case _ => MyCons(head, l)
    }
  }

  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    def go(idx: Int, remainder: MyList[A]): MyList[A] = 
      if(idx <= 0 || remainder == MyNil) remainder
      else go(idx - 1, tail(remainder))

    go(n, l)
  }

  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = {
    l match {
      case MyCons(h, t) => if(f(h)) dropWhile(t, f) else l
      case MyNil => l  
    }
  }
}
