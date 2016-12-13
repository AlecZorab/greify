package greify

import scala.language.higherKinds
import cats._
import cats.implicits._

object Tests {

  def main(args: Array[String]) = {
    test1()
    test2()
  }

  def test1() : Unit = {

    case class Mu[F[_]](v: () => F[Mu[F]])
    def mu[F[_]](v: => F[Mu[F]]) = Mu(() => v)

    implicit def muT[A[_]: Traverse]: MuRef.Aux[Mu[A], A] = new MuRef[Mu[A]] {
      type Out[X] = A[X]
      def mapDeRef[F[_]: Applicative, B](f: Mu[A] => F[B], a: Mu[A]): F[A[B]] =
        a.v() traverse f
    }

    sealed trait ABList[A, B]
    case class ABCons[A, B](a: () => A, b: () => B) extends ABList[A, B]{
      override def toString = s"${a()} :: ${b()}"
    }
    def abcons[A, B](a: => A, b: => B) = ABCons(() => a, () => b)
    case class ABNil[A, B]() extends ABList[A, B]


    implicit def traverseABList[A] : Traverse[({type λ[α] = ABList[A, α]})#λ] = {
      type F[X] = ABList[A, X]
      new Traverse[F] {
        override def map[A, B](fa: F[A])(f: A => B): F[B] = fa match {
          case ABNil() => ABNil()
          case ABCons(l, r) => ABCons(l, () => f(r()))
        }
        def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = fa match {
          case ABNil() => b
          case ABCons(_, a) => f(b, a())
        }
        def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) = fa match {
          case ABNil() => lb
          case ABCons(_, a) => f(a(), lb)
        }
        def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]) : G[F[B]] = fa match {
          case ABNil() => (ABNil(): F[B]).pure[G]
          case ABCons(l, r) => f(r()).map(fr => ABCons(l, () => fr)) 
        }
      }
    }

    type MuList[A] = Mu[({ type λ[α] = ABList[A, α] })#λ]
    val g1 : MuList[Int] = mu(abcons(1, mu(abcons(2, mu(ABNil())))))
    println(MuRef.reifyGraph(g1).toString)
    lazy val g2: MuList[Int] = mu(abcons(1, mu(abcons(2, g2))))
 
    println(MuRef.reifyGraph(g2).toString)

    def count(i:Int): MuList[Int]  = if (i == 0) mu(ABNil()) else mu(abcons(i, count(i-1)))

    println(MuRef.reifyGraph(count(10)).toString)
  }

  def test2(): Unit = {


    case class State[A, B](a: () => A, bs: () => List[(B, State[A, B])]){
      override def toString = s"State(${a()}, ${bs()}"
    }
    def state[A, B](a: => A, bs: => List[(B, State[A, B])]) = State(() => a, () => bs)
    
    lazy val s0: State[Int, Boolean] = state(0, (true, s1) :: (false, s2) :: Nil)
    lazy val s1: State[Int, Boolean] = state(1, (true, s0) :: (false, s1) :: Nil)
    lazy val s2: State[Int, Boolean] = state(2, (true, s1) :: (false, s0) :: Nil)

    case class StateDeRef[A, B, R](a:A, bs: List[(B, R)]){
      override def toString = s"StateDeRef(${a}, ${bs})"
    }
    implicit def stateMuRef[A, B]: MuRef.Aux[State[A, B], ({ type λ[α] = StateDeRef[A, B, α] })#λ] = 
      new MuRef[State[A, B]] {
        type Out[X] = StateDeRef[A, B, X]
        def mapDeRef[F[_]:Applicative, C](f: State[A, B] => F[C], a: State[A, B]): F[StateDeRef[A, B, C]] = 
          a match {
            case State(a, tr) => tr().traverse{case (b, s) => f(s).map((b, _))}.map(StateDeRef(a(), _))
          }
      }
      println(MuRef.reifyGraph(s0))
  }
}
