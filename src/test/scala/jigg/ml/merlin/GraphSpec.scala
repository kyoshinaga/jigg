package jigg.ml.merlin

/**
  * Created by kenta-yoshinaga on 2016/11/10.
  */

import org.scalatest._
import breeze.linalg.{DenseMatrix, DenseVector}
import jigg.util.hdfutil.{H5Elem, H5Util}

class GraphSpec extends FlatSpec with Matchers{

  "constructGraph" should "throw exception when invalid H5Node input" in {
    val rootElem = H5Elem("ROOT","Root")
    val copyElem = rootElem.copy(child = Seq(H5Elem("Dummy","Group")))
    val copyMerlin = rootElem.copy(child = Seq(H5Elem("Merlin","Group")))

    an[IllegalAccessError] should be thrownBy Graph.constructGraph(rootElem)
    an[IllegalAccessError] should be thrownBy Graph.constructGraph(copyElem)
    an[IllegalAccessError] should be thrownBy Graph.constructGraph(copyMerlin)

    1 should be (1)
  }

  "constructGraph" should "return list of Functor" in {

    val doc = H5Util.loadData("./target/test-classes/data/merlin/tokenizer_test.h5")

    val functors = Graph.constructGraph(doc)

    1 should be (1)
  }

}
