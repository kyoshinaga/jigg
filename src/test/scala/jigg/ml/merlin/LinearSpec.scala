package jigg.ml.merlin

/**
  * Created by kenta-yoshinaga on 2016/11/10.
  */
import org.scalatest._
import breeze.linalg.DenseMatrix
import breeze.numerics.abs
import jigg.util.hdfutil.H5Util

class LinearSpec extends FlatSpec with Matchers{

  "Linear.convert" should "match gold matrix of model file" in {
    val embFile = "./target/test-classes/data/merlin/tokenizer_test_embedding.h5"
    val cvFile = "./target/test-classes/data/merlin/tokenizer_test_convolution.h5"
    val lsFile = "./target/test-classes/data/merlin/tokenizer_test_linear.h5"
    val lsConvertFile = "./target/test-classes/data/merlin/tokenizer_test_linear_converted.h5"

    val embModel = H5Util.loadData(embFile).child.head
    val cvModel = H5Util.loadData(cvFile).child.head
    val lsModel = H5Util.loadData(lsFile).child.head

    val embedding = Embedding(embModel)
    val conv = Conv(cvModel)
    val ls = Linear(lsModel)

    val inputData = DenseMatrix((0 until 20).toArray.map(_.toFloat))

    val convertedData = ls.convert(Transpose(conv.convert(embedding.convert(inputData))))

    val goldData = H5Util.loadData(lsConvertFile).child.head.child(1)

    val goldMatrix = DenseMatrix.zeros[Float](3,20)
    for (y <- 0 until 3)
      for (x <- 0 until 20)
        goldMatrix(y, x) = goldData(x, y).asInstanceOf[Float]

    val diff = abs(convertedData - goldMatrix).forall(x => x < 1e-5)

    diff should be (true)
  }
}
