package jigg.ml.merlin

import breeze.linalg.DenseMatrix

/**
  * Created by kenta-yoshinaga on 2016/10/04.
  */
trait Functor {

  def functorName:String

  def convert(data: DenseMatrix[Float]): DenseMatrix[Float]

}

