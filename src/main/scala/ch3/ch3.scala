
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
 
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def map[A,B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => append2(f(h), flatMap(t)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
  }

  def flatFilter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] =
    as match {
      case Nil => Nil
      case Cons(a, at) => bs match {
        case Nil => Nil
        case Cons(b, bt) => Cons(f(a,b), zipWith(at, bt)(f))
      }
    }

  def cat[A](ls: List[List[A]]): List[A] =
    ls match {
      case Nil => Nil
      case Cons(h, t) =>
        foldRight(h, cat(t))((a, bs) => Cons(a, bs))
    }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil => sub == Nil
      case Cons(a, at) => sub match {
        case Nil => true
        case Cons(b, bt) =>
          if (a == b) hasSubsequence(at, bt)
          else hasSubsequence(at, sub)
      }
    }

  def append2[A](a1: List[A], a2: List[A]): List[A] = 
    foldRight(a1, a2)((a, bs) => Cons(a, bs))

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def addPair(as: List[Int], bs: List[Int]): List[Int] = as match {
    case Nil => Nil
    case Cons(a, at) => bs match {
      case Nil => Nil
      case Cons(b, bt) => Cons(a + b, addPair(at, bt))
    }
  }

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def toString(as: List[Int]): String =
    foldRight(as, "")( (a:Int, b:String) =>
      (if (b == "") "%d" else "%d, ").format(a) ++ b )

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil: List[A])((as, b) => Cons(b, as))

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((a, b) => b + 1)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((a, b) => a + 1)

  def tail[A](as: List[A]): List[A] = as match {
    case Cons(_, b) => b
    case Nil => Nil
  }

  def setHead[A](as: List[A], h: A) = as match {
    case Nil => Cons(h, Nil)
    case Cons(a, b) => Cons(h, b)
  }

  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Cons(_, b) => if (n == 0) as else drop(b, n-1)
    case Nil => Nil
  }

  def dropWhile[A](as: List[A], fn: A => Boolean): List[A] = as match {
    case Cons(a, b) => if ( fn(a) ) dropWhile(b, fn) else as
    case Nil => Nil
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def add1(as: List[Int]): List[Int] =
    map(as)(_ + 1)

  def stringElems(as: List[Double]): List[String] = as match {
    case Nil => Nil
    case Cons(a, b) => Cons(a.toString(), stringElems(b))
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    println(List.tail(List(1,2,3,4)))
    println(List.setHead(List(1,2,3), 0))
    println(List.drop(List(1,2,3,4), 2))
    println(List.dropWhile( List(1,2,3,4), (_:Int) < 3 ))

    println("init", List.init( List(1,2,3,4) ))

    println(List.toString(List(1,2,3,4)))

    println("apply: ", List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
    println("len2: ", List.length2( List(1,2,3) ))

    println("sum3 ", List.sum3(List(1,2,3)))

    println("reverse ", List.reverse(List(1,2,3)))

    println("cat ", List.cat( List( List(1,2,3), List(4,5,6), List(7,8,9) )))

    println("append2 ", List.append2( List(1,2,3), List(4,5,6) ))

    println("add1", List.add1(List(1,2,3)))

    println("string elems ", List.stringElems(List(1,2,3)))

    println("filter > 3", List.filter(List(1,2,3,4))(_ > 3))

    println("flatMap ", List.flatMap(List(1,2,3))(i => List(i,i)))
    println("flatFilter < 3", List.flatFilter(List(1,2,3,4))(_ < 3) )

    println("zip add", List.addPair(List(1,2), List(4,5,6)))

    println("zipWith", List.zipWith(List(1,2,3), List("is one", "is two", "is three"))(
      (a, b) => a.toString ++ " " ++ b
    ))

    println("hasSubsequence:\n", List.map(
      List(
        List( List(1,2,3), List(1,2) ),
        List( List(1,2,3), List(3) ),
        List( List(1,2,3), List(2) ),
        List( List(1,2,3), List(1,2,3,4) ),
        List( List(1,2,3), List(3,4) ),
      ))(
        as => as match {
          case Cons(a,Cons(b, _)) => "\t%s hasSubsequence? %s = %b\n".format(a, b,
            List.hasSubsequence(a, b))
          case _ => Nil
        }
      ))
  }
}