package jigg.ml.merlin

import org.scalatest._
import breeze.linalg.DenseMatrix
import breeze.numerics.abs
import jigg.util.hdfutil._

/**
  * Created by ubuntu on 10/18/16.
  */
class ConvSpec extends FlatSpec with Matchers{

  "Convolution.convert" should "match gold matrix of model file" in {
    val embFile = "./target/test-classes/data/merlin/tokenizer_test_embedding.h5"
    val cvFile = "./target/test-classes/data/merlin/tokenizer_test_convolution.h5"
    val cvConvertFile = "./target/test-classes/data/merlin/tokenizer_test_convolution_converted.h5"

    val embModel = H5Util.loadData(embFile).child.head
    val cvModel = H5Util.loadData(cvFile).child.head
    val goldData = H5Util.loadData(cvConvertFile).child.head.child(1)

    val embedding = Embedding(embModel)
    val conv = Conv(cvModel)

    val inputData = DenseMatrix((0 until 20).toArray.map(_.toFloat))

    val convertedData = conv.convert(embedding.convert(inputData))

    val goldMatrix = DenseMatrix.zeros[Float](20,4)
    for (y <- 0 until 20)
      for (x <- 0 until 4)
        goldMatrix(y, x) = goldData(x, y).asInstanceOf[Float]

    val diff = abs(convertedData - goldMatrix).forall(x => x < 1e-5)

    diff should be (true)
  }
}
