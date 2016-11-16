package jigg.ml.merlin

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import jigg.util.hdfutil.H5Node

/**
  * Created by kenta-yoshinaga on 2016/08/30.
  */
class Embedding private(val vocabulary: Int, val outdim: Int) extends Functor{

  override def functorName: String = "Embedding"

  private val w = new Array[DenseVector[Float]](vocabulary).map(_ => DenseVector.zeros[Float](outdim))

  protected def h5load(data: H5Node): Unit = {
    for(y <- 0 until data.dims.head.toInt)
      for(x <- 0 until data.dims(1).toInt)
        w(y)(x) = data(y,x).asInstanceOf[Float]
  }

  override final def convert(data: DenseMatrix[Float]): DenseMatrix[Float] = {
    val arrayOfId = data.reshape(1, data.size)
    val length = arrayOfId.size
    val z = DenseMatrix.zeros[Float](outdim, length)
    for ( i <- 0 until length){
      z(::, i) := w(arrayOfId(0,i).asInstanceOf[Int])
    }
    z
  }
}

object Embedding {

  def apply(vocabulary:Int, outdim: Int):Embedding = new Embedding(vocabulary, outdim)

  def apply(h5node: H5Node): Embedding = {
    val emb = new Embedding(h5node.child(1).dims.head.toInt, h5node.child(1).dims(1).toInt)
    emb.h5load(h5node.child(1))
    emb
  }

  def unapply(e: Embedding) = Option((e. vocabulary, e.outdim))
}

