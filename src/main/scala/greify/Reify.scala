package greify

import java.util

import scala.language.higherKinds
import cats._
import cats.implicits._

case class Graph[F[_]](edges: Array[F[Int]]) {
  override def toString = s"Graph(${edges.mkString(", ")})"
}

trait MuRef[A] {
  type Out[_]
  def mapDeRef[F[_]: Applicative, B](f: A => F[B], a: A): F[Out[B]]
}

object MuRef {
  type Aux[A, B[_]] = MuRef[A] { type Out[X] = B[X] }

  def reifyGraph[S, F[_]](s: S)(implicit ev: MuRef.Aux[S, F], ct: reflect.ClassTag[F[Int]]): Graph[F] = {
    import scala.collection.JavaConverters._
    val positions = new util.IdentityHashMap[S, Int]().asScala
    var indices     = List.empty[(Int, F[Int])]
    var ctr         = 0
    def findNodes(s: S): Id[Int] = {
      positions get s match {
        case Some(i) => i
        case None =>
          val i = ctr
          ctr += 1
          positions(s) = i
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
