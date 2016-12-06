package jigg.ml.merlin

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import jigg.util.hdfutil.H5Node

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

  def unapply(e: Embedding) = Option((e.vocabulary, e.outdim))
}

