package jigg.util.hdfutil

/**
  * Created by kenta-yoshinaga on 2016/11/09.
  */

import org.scalatest._

class H5DataSetSpec extends FlatSpec with Matchers{

  "apply" should "throw exception when empty data input" in {
    an[IllegalArgumentException] should be thrownBy H5DataSet(Nil,"dummy",-1,Seq(Nil.length))
  }

}
