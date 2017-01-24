package jigg.pipeline

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

import jigg.ml.keras.KerasParser
import jigg.util.XMLUtil

import scala.xml.Node

class SsplitKerasAnnotator(override val name: String, override val props: Properties) extends Annotator {

  def defaultModelFileName = "model.h5"
  def defaultTableFileName = "table.json"

  @Prop(gloss = "Model file (if omitted, the default path is used to search file)") var model = ""
  @Prop(gloss = "Lookup table for mapping character into id space") var table = ""

  readProps()

  lazy val ssplitter: QueueSsplitter = new QueueSsplitter

  override def description =s"""${super.description}

  A sentence splitter using model file generated by keras.

"""

  override def init() = {
    ssplitter
  }

  private[this] val sentenceIDGen = jigg.util.IDGenerator("s")


  override def annotate(annotation: Node): Node = {

    XMLUtil.replaceAll(annotation, "document") { e =>
      val line = e.text
      val sentenceBoundaries = ssplitter.s.parsing(line)
      val sentences: Vector[Node] = sentenceBoundaries.flatMap{x =>
        val sentence: String = line.substring(x._1, x._2)
        if (sentence.isEmpty)
          None
        else {
          Option(<sentence
            id={ sentenceIDGen.next }
            characterOffsetBegin={ x._1 + ""}
            characterOffsetEnd={ x._2 + ""} >{ sentence }</sentence>
            )
        }
      }.toVector
      XMLUtil.addChild(e, <sentences>{ sentences }</sentences>)
    }

    annotation
  }

  class QueueSsplitter {
    private def mkSsplitter: KerasParser = model match {
      case "" =>
        System.err.println(s"No model file is given. Try to search default path: ${defaultModelFileName}")
        table match {
          case "" =>
            System.err.println(s"No lookup table file is given. Try to search default path: ${defaultModelFileName}")
            KerasParser(defaultModelFileName, defaultTableFileName)
          case tableFile =>
            KerasParser(defaultModelFileName, tableFile)
        }
      case modelFile =>
        table match {
          case "" =>
            System.err.println(s"No lookup table file is given. Try to search default path: ${defaultModelFileName}")
            KerasParser(model, defaultTableFileName)
          case tableFile =>
            KerasParser(model, tableFile)
        }
    }

    val s = mkSsplitter
  }

  override def requires = Set()
  override def requirementsSatisfied(): Set[Requirement] = Set(Requirement.Ssplit)

}

object SsplitKerasAnnotator extends AnnotatorCompanion[SsplitKerasAnnotator] {

  override def fromProps(name: String, props: Properties) = {
    val modelFile = name + ".model"
    val tableFile = name + ".table"
  }

}