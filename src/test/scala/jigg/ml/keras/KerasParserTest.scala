package jigg.ml.keras

/*
 Copyright 2013-2015 Hiroshi Noji
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at
 
     http://www.apache.org/licencses/LICENSE-2.0
     
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitation under the License.
*/

import java.util.Properties

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class KerasParserTest extends FunSuite{

  def findPath(localPath: String) = getClass.getClassLoader.getResource(localPath).getPath

  test("get Offset list") {

    val modelPath = findPath("./data/keras/ssplit_model.h5")
    val tablePath = findPath("./data/keras/jpnLookup.json")

    val parser = KerasParser(modelPath, tablePath)

    val tags = List[Int](0,1,1,0,1,1)

    val ranges = parser.getOffsets(tags)

    println(ranges.mkString(",\n"))

    1 should be (1)
  }

}
