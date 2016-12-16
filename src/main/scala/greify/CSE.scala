package greify

import cats._
import cats.implicits._
import scala.language.higherKinds

import scala.collection.mutable

object CSE {

  class NodeRepr[F[_]](val shape: F[Unit], val links: Array[Int]) {
    override def equals(other: Any): Boolean = other match {
      case that: NodeRepr[r] if shape == that.shape && links.length == that.links.length =>
        var same = true
        var i    = 0
        val max  = links.length
        while (same && i < max) {
          same = links(i) == that.links(i)
          i += 1
        }
        same
      case _ => false
    }
    override val hashCode: Int = {
      val a = shape.##
      val b = scala.util.hashing.MurmurHash3.arrayHash(links)
      31 * a + b
    }

    def toNode(implicit ev: Traverse[F]) = {
      val ab = mutable.ArrayBuffer(links: _*)
      shape map (_ => ab remove 0)
    }
  }

  object NodeRepr {
    def apply[F[_]: Traverse](f: F[Int]): NodeRepr[F] = {
      val shape = f map (_ => ())
      val links = f.foldLeft(mutable.ArrayBuffer.empty[Int])(_ += _).toArray
      NodeRepr(shape, links)
    }
    def apply[F[_]](shape: F[Unit], links: Array[Int]): NodeRepr[F] =
      new NodeRepr(shape, links)
  }

  def cse[F[_] <: AnyRef: Traverse](g: Graph[F])(implicit ev1: reflect.ClassTag[F[Int]]): Graph[F] = {
    val reprs: Array[NodeRepr[F]] = g.edges.map(NodeRepr(_))
    val csebits                   = cseImpl(reprs)
    Graph(csebits.map(_.toNode))
  }

  def cseImpl[F[_] <: AnyRef: Traverse](reprs: Array[NodeRepr[F]]): Array[NodeRepr[F]] = {
    import collection.{mutable => m}

    type AB[X] = m.ArrayBuffer[X]
    val AB = m.ArrayBuffer

    type NodeIdx  = Int
    type DepthIdx = Int

    //walk through tree marking each node with reverse depth (leaves = 0)
    val depths = Array.fill(reprs.length)(-1)
    def visit(idx: NodeIdx): Unit = {
      if (depths(idx) == -1) {
        val outEdges = reprs(idx).links
        //make sure the children have been visited
        outEdges foreach visit
        //0 if I have no children
        val myDepth = outEdges.map(depths).foldLeft(-1)(_ max _) + 1
        depths(idx) = myDepth
      }
    }
    visit(0)

    //bucket them
    //the root definitely has to be done last, so we know it's the highest idx
    val buckets = {
      val b = Array.fill(depths(0) + 1)(AB.empty[NodeIdx])
      var j = 0
      while (j < depths.length) {
        val depth = depths(j)
        b(depth) += j
        j += 1
      }
      b
    }

    //reassemble in order

    val newIdxs = Array.ofDim[Int](reprs.length)
    val newData = m.AnyRefMap.empty[NodeRepr[F], NodeIdx]
    var ctr     = 0
    def getIdxOrInsert(n: NodeRepr[F]) = newData get n match {
      case None =>
        val idx = ctr
        newData(n) = idx
        ctr += 1
        idx
      case Some(i) =>
        i
    }

    var b = 0
    while (b < buckets.length) {
      val data = buckets(b)
      var j    = data.size - 1
      //run through the bucket backwards here, so that when we reverse direction later they come out with increasing indices
      while (j >= 0) {
        val oldIdx   = data(j)
        val d        = reprs(oldIdx)
        val shape    = d.shape
        val oldLinks = d.links

        //the newId entry for our dependencies should always have been populated by the time we're looking for it
        val updatedLinks = oldLinks map newIdxs
        val newIdx       = getIdxOrInsert(NodeRepr(shape, updatedLinks))
        newIdxs(oldIdx) = newIdx
        j -= 1
      }
      b += 1
    }

    //reverse and rebuild F[Int]
    val resultArr = Array.ofDim[NodeRepr[F]](ctr)
    def f(i: Int) = ctr - i - 1
    for (e <- newData) {
      val (r, i) = e
      resultArr(f(i)) = NodeRepr(r.shape, r.links map f)
    }

    resultArr
  }
}
