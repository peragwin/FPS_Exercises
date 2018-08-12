
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
        case Left(e) => Left(e)
        case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
        this match {
            case Left(e) => Left(e)
            case Right(a) => f(a)
        }

    def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B] =
        this match {
            case Left(_) => b
            case _ => this
        }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] =
        for {
            aa <- this
            bb <- b
        } yield f(aa, bb)
}

object Main {
    def Try[A](a: => A): Either[Exception, A] =
        try Right(a)
        catch { case e: Exception => Left(e) }
    
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
        es.foldRight(Right(List()): Either[E, List[A]])((ee, ees) => ee.map2(ees)(_ :: _))

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
        as.foldRight(Right(List()): Either[E, List[B]])(
            (a, es) => for {
                eb <- f(a)
                ess <- es
            } yield eb :: ess)

    def ex4_7(): Unit = {
        println("ex4_7 sequence")
        var es: List[Either[String, Int]] = List(Right(1), Right(2), Right(3))
        println(sequence(es))
        es = List(Right(1), Left("nope"), Right(3))
        println(sequence(es))

        println("ex4_7 traverse")
        var as = List("1", "2", "3")
        println(traverse(as)(a => Try(a.toInt)))
        as = List("1", "nope", "3")
        println(traverse(as)(a => Try(a.toInt)))
    }

    // ex4_8
    // define a new datatype like Any3[Left, Middle, Right], otherwise you have to nest Eithers
    // within the Left of Either. `orElse` on Any3 would have to take two arguments, one for each
    // Left and Middle case.

    def main(args: Array[String]): Unit = {
        ex4_7()
    }
}