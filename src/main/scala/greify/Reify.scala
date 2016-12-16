package greify

import scala.language.higherKinds
import cats._
import cats.implicits._
import collection.mutable.AnyRefMap

case class Graph[F[_]](edges: Array[F[Int]]) {
  override def toString = s"Graph(${edges.mkString(", ")})"
}

case class StableName(val a: AnyRef) {
  override def hashCode = System.identityHashCode(a)
  override def toString = s"StableName@$hashCode"
  override def equals(other: Any) = other match {
    case that: StableName => that.a eq this.a
    case _                => false
  }
}

trait MuRef[A] {
  type Out[_]
  def mapDeRef[F[_]: Applicative, B](f: A => F[B], a: A): F[Out[B]]
}

object MuRef {
  type Aux[A, B[_]] = MuRef[A] { type Out[X] = B[X] }

  def reifyGraph[S <: AnyRef, F[_]](s: S)(implicit ev: MuRef.Aux[S, F], ct: reflect.ClassTag[F[Int]]): Graph[F] = {
    val stableNames = AnyRefMap.empty[StableName, Int]
    var indices     = List.empty[(Int, F[Int])]
    var ctr         = 0
    def findNodes(s: S): Id[Int] = {
      val name = StableName(s)
      stableNames get name match {
        case Some(i) => i
        case None =>
          val i = ctr
          ctr += 1
          stableNames(name) = i
          val res = ev.mapDeRef(findNodes, s)
          indices ::= (i, res)
          i
      }
    }
    findNodes(s)
    val res = Array.ofDim[F[Int]](ctr)
    for(i <- indices) res(i._1) = i._2
    Graph(res)
  }
}
