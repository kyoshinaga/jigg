package jigg.ml.merlin

import breeze.linalg.{DenseMatrix, argmax}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by kenta-yoshinaga on 2016/10/19.
  */
class TokenizerSpec extends FlatSpec with Matchers {


  "Tokenizer" should "annotated string" in {

    val testString = "上海ガニをばくばく食べる。"
    val goldStringList = List("上海","ガニ","を","ばくばく","食べる","。")

    val filePath = "./target/test-classes/data/merlin/tokenizer_test.h5"

    val t = Tokenizer(filePath)

    val testStringList = t.tokenize(testString).split("\n").toList

    (testStringList zip goldStringList).forall(x => x._1 == x._2) should be (true)
  }

}
