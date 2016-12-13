package greify

import scala.language.higherKinds
import cats._
import cats.implicits._

object Tests {

  def main(args: Array[String]) = {
    test1()
    test2()
  }

  def test1(): Unit = {

    class Mu[F[_]](vv: => F[Mu[F]]) {
      lazy val v = vv
    }
    def mu[F[_]](v: => F[Mu[F]]) = new Mu(v)

    implicit def muT[A[_]: Traverse]: MuRef.Aux[Mu[A], A] = new MuRef[Mu[A]] {
      type Out[X] = A[X]
      def mapDeRef[F[_]: Applicative, B](f: Mu[A] => F[B], a: Mu[A]): F[A[B]] =
        a.v traverse f
    }

    sealed trait ABList[A, B] {
      def fold[T](onNil: => T, onCons: (=> A, => B) => T): T
    }

    class ABCons[A, B](aa: => A, bb: => B) extends ABList[A, B] {
      lazy val a = aa
      lazy val b = bb

      final def fold[T](onNil: => T, onCons: (=> A, => B) => T) = onCons(a, b)
      override def toString                                     = s"$a :: $b"
    }

    def abcons[A, B](a: => A, b: => B) = new ABCons(a, b)

    case class ABNil[A, B]() extends ABList[A, B] {
      final def fold[T](onNil: => T, onCons: (=> A, => B) => T) = onNil
    }

    implicit def traverseABList[A]: Traverse[({ type λ[α] = ABList[A, α] })#λ] = {
      type F[X] = ABList[A, X]
      new Traverse[F] {
        def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = fa.fold(b, (_, a) => f(b, a))

        def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) = fa.fold(lb, (_, a) => f(a, lb))

        def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
          fa.fold((ABNil(): F[B]).pure[G], (l, r) => f(r).map(fr => abcons(l, fr)))

      }
    }

    type MuList[A] = Mu[({ type λ[α] = ABList[A, α] })#λ]
    val g1: MuList[Int] = mu(abcons(1, mu(abcons(2, mu(ABNil())))))
    println(MuRef.reifyGraph(g1).toString)
    lazy val g2: MuList[Int] = mu(abcons(1, mu(abcons(2, g2))))

    println(MuRef.reifyGraph(g2).toString)

    def count(i: Int): MuList[Int] = if (i == 0) mu(ABNil()) else mu(abcons(i, count(i - 1)))

    println(MuRef.reifyGraph(count(10)).toString)
  }

  def test2(): Unit = {

    class State[A, B](aa: => A, bb: => List[(B, State[A, B])]) {
      lazy val a            = aa
      lazy val bs           = bb
      override def toString = s"State($a, $bs)"
    }

    def state[A, B](a: => A, bs: => List[(B, State[A, B])]) = new State(a, bs)

    type SIB = State[Int, Boolean]
    lazy val (s0: SIB, s1: SIB, s2: SIB) =
      (state(0, (true, s1) :: (false, s2) :: Nil),
       state(1, (true, s0) :: (false, s1) :: Nil),
       state(2, (true, s1) :: (false, s0) :: Nil))

    case class StateDeRef[A, B, R](a: A, bs: List[(B, R)]) {
      override def toString = s"StateDeRef(${a}, ${bs})"
    }

    implicit def stateMuRef[A, B]: MuRef.Aux[State[A, B], ({ type λ[α] = StateDeRef[A, B, α] })#λ] =
      new MuRef[State[A, B]] {
        type Out[X] = StateDeRef[A, B, X]
        def mapDeRef[F[_]: Applicative, C](f: State[A, B] => F[C], a: State[A, B]): F[StateDeRef[A, B, C]] =
          a.bs traverse (t => f(t._2) map ((t._1, _))) map (StateDeRef(a.a, _))
      }
    println(MuRef.reifyGraph(s0))
  }
}
