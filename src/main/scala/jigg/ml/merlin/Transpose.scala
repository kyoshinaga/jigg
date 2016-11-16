package jigg.ml.merlin
import breeze.linalg.DenseMatrix

/**
  * Created by kenta-yoshinaga on 2016/10/19.
  */
object Transpose extends Functor{

  override def functorName: String = "Transpose"

  override def convert(data: DenseMatrix[Float]): DenseMatrix[Float] = data.t

  def apply(m: DenseMatrix[Float]) = this.convert(m)

}
