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

import breeze.linalg.{DenseMatrix, argmax}
import jigg.util.hdfutil._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Tokenizer (val model: H5Node, val fid: Int){

  if(model == null)
    throw new IllegalArgumentException("cannot construct " + getClass.getSimpleName + " with null")

  private val functors = Graph.constructGraph(model)

  class iddict(val dict: H5Node){
    val id2count = dict.child(1).data
    val id2key = dict.child(2).data
    val key2id:mutable.Map[String,Int] = {
      val tempMap = mutable.Map[String, Int]()
      for (c <- dict.child(3).child){
        tempMap += (c.label -> c.data.head.asInstanceOf[Int])
      }
      tempMap
    }
  }

  private val dict = new this.iddict(model.child.head.child(1))

  private val tagset: mutable.Map[Int, String] = {
    val mappingId = mutable.Map[Int,String]()
    for(c <- model.child.head.child(3).child.filterNot(_.label == "#TYPE")){
      mappingId(c.data.head.asInstanceOf[Int] - 1) = c.label
    }
    mappingId
  }

  def tokenize(str: String) : String = {
    val outputData = callFunctors(decode(str), functors)
    val tags = classification(outputData)
    val decodedString = decode(tags)
    decodedString.map(x => str.substring(x._1, x._2)).mkString("\n")
  }

  def tokenizeWithRanges(str: String) : List[(String,(Int,Int))] = {
    val outputData = callFunctors(decode(str), functors)
    val tags = classification(outputData)
    val ranges = decode(tags)
    val words = ranges.map(x => str.substring(x._1, x._2))
    words zip ranges
  }

  private def decode(str: String) : DenseMatrix[Float] = {
    val strArray = str.toCharArray.map(x => dict.key2id.getOrElse(x.toString, 1) - 1.0.toFloat)
    val inputData = DenseMatrix(strArray)
    inputData
  }

  private def decode(tags: List[Int]): List[(Int, Int)] = {
    var bpos = -1
    val ranges = ListBuffer[(Int,Int)]()
    for (i <- tags.indices){
      tagset(tags(i)) match {
        case "I" =>
          if (bpos == -1)
            bpos = i
        case "E" =>
          if (bpos == -1)
            ranges += ((i,i+1))
          else
            ranges += ((bpos,i+1))
          bpos = -1
        case "O" =>
          ranges += ((i, i+1))
          bpos = -1
      }
    }
    ranges.toList
  }

  private def classification(x: DenseMatrix[Float]): List[Int] = {
    for {
      i <- 0 until x.cols
      maxI = argmax(x(::,i))
    }yield maxI
  }.toList

  private def callFunctors(input: DenseMatrix[Float], unprocessed:List[Functor]): DenseMatrix[Float] = unprocessed match {
    case functor :: tail =>
      val interOutput = functor.convert(input)
      callFunctors(interOutput, tail)
    case Nil => input
  }

  def close(): Unit = H5Util.closeFile(fid)
}

object Tokenizer {
  def apply(path: String) : Tokenizer = {
    val fid = H5Util.openFile(path)
    val model = H5Util.loadData(fid)
    new Tokenizer(model,fid)
  }
}
