package jigg.ml.merlin

/*
 Copyright 2013-2015 Hiroshi Noji

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import breeze.linalg.DenseMatrix
import jigg.util.hdfutil.H5Node

class Conv private (filterRow: Int, filterCol:Int,
           inCh: Int, outCh: Int,
           strideRow: Int, strideCol: Int,
           paddingRow: Int, paddingCol: Int) extends Functor{
  override def functorName: String = "Conv"

  private val w = Array.ofDim[DenseMatrix[Float]](inCh, outCh).map(
    _.map(
      _ => {DenseMatrix.zeros[Float](filterRow, filterCol)}
    )
  )

  private val filterDim = (filterRow, filterCol)
  private val stride = (strideRow, strideCol)
  private val padding = (paddingRow, paddingCol)

  def getW = w

  protected def h5load(data: H5Node): Unit = {
    for(outc <- 0 until data.dims.head.toInt)
      for(inc <- 0 until data.dims(1).toInt)
        for(y <- 0 until data.dims(3).toInt)
          for(x <- 0 until data.dims(2).toInt)
            w(inc)(outc)(y,x) = data(outc, inc, x, y).asInstanceOf[Float]
  }

  override final def convert(data: DenseMatrix[Float]): DenseMatrix[Float] = {
    val filterSize = filterRow * filterCol
    val chSize = inCh * outCh
    val outdims = outsize(data)
    val ws2col = DenseMatrix.zeros[Float](filterSize, chSize)

    for(j <- 0 until inCh){
      for(i <- 0 until outCh){
        val index = j * outCh + i
        ws2col(::,index) := w(j)(i).toDenseVector
      }
    }

    val work = im2col(data, outdims)

    work * ws2col
  }

  private def im2col(x: DenseMatrix[Float], outdims: Array[Int]): DenseMatrix[Float] = {
    val filterSize = filterRow * filterCol
    val work = DenseMatrix.zeros[Float](outdims.product, filterSize)

    val x1 = x.rows
    val x2 = x.cols
    val w1 = filterRow
    val w2 = filterCol
    val s1 = strideRow
    val s2 = strideCol
    val p1 = paddingRow
    val p2 = paddingCol
    val n1 = (x1 + 2 * p1 - w1) / s1 + 1
    val n2 = (x2 + 2 * p2 - w2) / s2 + 1

    for (d2 <- 0 until w2){
      for (d1 <- 0 until w1){
        for(k2 <- 0 until n2){
          for(k1 <- 0 until n1){
            val i1 = k1 * s1 - p1 + d1
            val i2 = k2 * s2 - p2 + d2
            val j1 = d1 + d2 * w1
            val j2 = k2
            if(i1 >= 0 & i1 < x1 & i2 >= 0 & i2 < x2){
              work(j2, j1) = x(i1, i2)
            }
            else{
              work(j2, j1) = 0.0.toFloat
            }
          }
        }
      }
    }
    work
  }

  private def outsize(x: DenseMatrix[Float]): Array[Int] = {
    val N = 2
    val dims = Array.ofDim[Int](N)
    dims(0) = (x.rows +  2 * paddingRow - filterRow) + strideRow
    dims(1) = (x.cols +  2 * paddingCol - filterCol) + strideCol
    dims
  }
}

object Conv {
  def apply(filter: Tuple2[Int,Int], ch: Tuple2[Int,Int],
            stride: Tuple2[Int,Int], padding: Tuple2[Int, Int]):Conv =
    new Conv(filter._1, filter._2, ch._1, ch._2, stride._1, stride._2,
      padding._1, padding._2)

  def apply(h5node: H5Node): Conv = {
    val filterDims = h5node.child(1)
    val paddims = h5node.child(2)
    val stride = h5node.child(3)
    val w = h5node.child(4)
    val conv = Conv(
      (filterDims.child(1).data.head.asInstanceOf[Int],
        filterDims.child(2).data.head.asInstanceOf[Int]),
      (w.child(1).dims(1).toInt,
        w.child(1).dims.head.toInt),
      (stride.child(1).data.head.asInstanceOf[Int],
        stride.child(2).data.head.asInstanceOf[Int]),
      (paddims.child(1).data.head.asInstanceOf[Int],
        paddims.child(2).data.head.asInstanceOf[Int])
    )
    conv.h5load(w.child(1))
    conv
  }
}