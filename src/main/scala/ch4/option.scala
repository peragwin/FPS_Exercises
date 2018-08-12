
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] =
        // this.map(f).getOrElse(None)
        this match {
            case None => None
            case Some(a) => f(a)
        }

    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(a) => a
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] =
        if (this == None) ob
        else this
    
    def filter(f: A => Boolean): Option[A] = {
        var fn = (a: A) => if (f(a)) Some(a) else None
        this.flatMap(fn)
    }
    // this match {
    //     case None => None
    //     case Some(a) => if (f(a)) this else None
    // }
}

object Main {
    def mean(xs: Seq[Double]): Option[Double] =
        if (xs.isEmpty) None
        else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] =
        mean(xs) flatMap (m => mean(xs map (x => math.pow(2, x-m))))

    def ex4_1(): Unit = {
        println("ex4_1")
        var xs = Seq(1.0,2,1,1.5)
        println("variance of %s is %f".format(xs, variance(xs).getOrElse(0.0)))
    }

    def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

    def Try[A](a: => A): Option[A] =
        try Some(a)
        catch { case e: Exception => None }

    // 4_3
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
        a flatMap (aa => b map (bb => f(aa, bb)))

    def sequence[A](as: List[Option[A]]): Option[List[A]] =
        (as foldRight (Some(List()): Option[List[A]])) ((e, l) => map2(e, l)(_ :: _))

    def ex4_4(): Unit = {
        println("ex4_4")
        var xs: List[Option[Int]] = List(Some(1), Some(2), Some(3))
        println(sequence(xs))
        xs = List(Some(1), Some(2), None)
        println(sequence(xs))
        xs = List()
        println(sequence(xs))
    }

    def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
        a.foldRight(Some(List()): Option[List[B]])((e, l) => l.flatMap(ll => f(e).map(_ :: ll)))

    def ex4_5(): Unit = {
        println("ex4_5")
        var xs = List("1", "2", "3")
        println(traverse(xs)(a => Try(a.toInt)))
        xs = List("1", "two", "3")
        println(traverse(xs)(a => Try(a.toInt)))
    }

    def main(args: Array[String]): Unit = {
        ex4_1()
        ex4_4()
        ex4_5()
    }
}
