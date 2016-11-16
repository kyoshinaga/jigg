package jigg.ml.merlin

import breeze.linalg.{DenseMatrix, DenseVector}
import jigg.util.hdfutil.H5Node

/**
  * Created by ubuntu on 10/17/16.
  */
class Linear private(indim: Int, outdim: Int) extends Functor{
  override def functorName: String = "Linear"

  private val w = DenseMatrix.zeros[Float](indim, outdim)

  private val b = DenseVector.zeros[Float](outdim)

  protected def h5load(W: H5Node, B: H5Node):Unit ={
    for(y <- 0 until W.dims.head.toInt)
      for(x <- 0 until W.dims(1).toInt) {
        w(y, x) = W(y, x).asInstanceOf[Float]
        if (y == 0){
          b(x) = B.data(x).asInstanceOf[Float]
        }
      }
  }

  override final def convert(x:DenseMatrix[Float]) = {
    val z = w.t * x
    for (i <- 0 until x.cols){
      z(::,i) := z(::,i) + b
    }
    z
  }
}

object Linear {

  def apply(indim:Int, outdim:Int) = new Linear(indim, outdim)

  def apply(h5node: H5Node): Linear = {
    val b = h5node.child(1)
    val w = h5node.child(2)
    val ls = new Linear(w.dims.head.toInt, w.dims(1).toInt)
    ls.h5load(w, b)
    ls
  }

}