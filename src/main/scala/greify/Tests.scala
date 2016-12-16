package greify

import scala.language.higherKinds
import cats._
import cats.implicits._

import scala.util.Random

object Tests {

  def main(args: Array[String]) = {
    test1()
    test2()
    test3()
  }

  def test1(): Unit = {

    sealed trait ABList[A, B] {
      def fold[T](onNil: => T, onCons: (=> A, => B) => T): T
    }

    class ABCons[A, B](aa: => A, bb: => B) extends ABList[A, B] {
      lazy val a = aa
      lazy val b = bb

      final def fold[T](onNil: => T, onCons: (=> A, => B) => T) = onCons(a, b)
      override def toString                                     = s"v = $a, next = #$b"
    }

    def abcons[A, B](a: => A, b: => B) = new ABCons(a, b)

    case class ABNil[A, B]() extends ABList[A, B] {
      final def fold[T](onNil: => T, onCons: (=> A, => B) => T) = onNil
    }

    implicit def traverseABList[A]: Traverse[({ type l[a] = ABList[A, a] })#l] = {
      type F[X] = ABList[A, X]
      new Traverse[F] {
        def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = fa.fold(b, (_, a) => f(b, a))

        def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) = fa.fold(lb, (_, a) => f(a, lb))

        def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
          fa.fold((ABNil(): F[B]).pure[G], (l, r) => f(r).map(fr => abcons(l, fr)))

      }
    }

    type MuList[A] = Mu[({ type l[a] = ABList[A, a] })#l]
    val g1: MuList[Symbol] = mu(abcons('a, mu(abcons('b, mu(ABNil())))))
    println(MuRef.reifyGraph(g1).toString)
    lazy val g2: MuList[Symbol] = mu(abcons('a, mu(abcons('b, g2))))

    println(MuRef.reifyGraph(g2).toString)

    def count(i: Int, max: Int): MuList[Int] = if (i == max) mu(ABNil()) else mu(abcons(i, count(i + 1, max)))

    println(MuRef.reifyGraph(count(0, 10)).toString)
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

    implicit def stateMuRef[A, B]: MuRef.Aux[State[A, B], ({ type l[a] = StateDeRef[A, B, a] })#l] =
      new MuRef[State[A, B]] {
        type Out[X] = StateDeRef[A, B, X]
        def mapDeRef[F[_]: Applicative, C](f: State[A, B] => F[C], a: State[A, B]): F[StateDeRef[A, B, C]] =
          a.bs traverse (t => f(t._2) map ((t._1, _))) map (StateDeRef(a.a, _))
      }
    println(MuRef.reifyGraph(s0))
  }

  class Mu[F[_]](vv: => F[Mu[F]]) {
    lazy val v = vv
  }

  def mu[F[_]](v: => F[Mu[F]]) = new Mu(v)

  implicit def muT[A[_]: Traverse]: MuRef.Aux[Mu[A], A] = new MuRef[Mu[A]] {
    type Out[X] = A[X]
    def mapDeRef[F[_]: Applicative, B](f: Mu[A] => F[B], a: Mu[A]): F[A[B]] =
      a.v traverse f
  }

  sealed trait Val[@specialized(Int) F] {
    def map[G](f: F => G): Val[G]
  }

  sealed trait Nonary[@specialized(Int) F] extends Val[F]

  case class Const[@specialized(Int) F](s: String) extends Nonary[F] {
    def as[G]: Val[G]             = this.asInstanceOf[Val[G]]
    def map[G](f: F => G): Val[G] = as[G]
  }

  sealed trait Binary[@specialized(Int) F] extends Val[F] {
    def build[G](g1: G, g2: G): Binary[G]
    final def isEmpty        = false
    final def get: Binary[F] = this
    def f1: F
    def f2: F
    final def _1: F = f1
    final def _2: F = f2
  }

  object Binary { def unapply[F](b: Binary[F]): Binary[F] = b }

  case class Mul[@specialized(Int) F](f1: F, f2: F) extends Binary[F] {
    def build[G](g1: G, g2: G)    = Mul(g1, g2)
    def map[G](f: F => G): Val[G] = Mul(f(f1), f(f2))
  }

  case class Add[@specialized(Int) F](f1: F, f2: F) extends Binary[F] {
    def build[G](g1: G, g2: G)    = Add(g1, g2)
    def map[G](f: F => G): Val[G] = Add(f(f1), f(f2))
  }

  object Val {
    implicit def traverse: Traverse[Val] = new Traverse[Val] {

      override def map[A, B](fa: Val[A])(f: A => B): Val[B] = fa map f

      def foldLeft[A, B](fa: Val[A], b: B)(f: (B, A) => B): B = fa match {
        case Const(_)       => b
        case Binary(a1, a2) => f(f(b, a1), a2)
      }

      def foldRight[A, B](fa: Val[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
        case Const(_)       => lb
        case Binary(a1, a2) => f(a1, f(a2, lb))
      }

      def traverse[G[_]: Applicative, A, B](fa: Val[A])(f: A => G[B]): G[Val[B]] = fa match {
        case c @ Const(_)       => c.as[B].pure[G]
        case b @ Binary(a1, a2) => f(a1) |@| f(a2) map b.build
      }
    }
  }

  def test3(): Unit = {
    type Term = Val[Mu[Val]]

    def add(v1: Term, v2: Term): Term = Add(mu(v1), mu(v2))
    def mul(v1: Term, v2: Term): Term = Mul(mu(v1), mu(v2))

    def mkTree(depth: Int, r: Random): Term = {
      def rConst(): Term                = Const(r.nextInt(10).toString)
      def rOp(t1: Term, t2: Term): Term = if (r.nextBoolean()) add(t1, t2) else mul(t1, t2)
      def inner(i: Int): Term           = if (i == 0) rConst() else rOp(inner(i - 1), inner(i - 1))
      inner(depth)
    }

    val ran = new Random(12345)
    for (i <- 3 until 12) yield {
      val aTree = mu(mkTree(i, ran))
      val gr    = MuRef.reifyGraph(aTree)

      println(CSE.cse(gr).edges.size)
    }
  }
}
