package jigg.pipeline

/*
 Copyright 2013-2015 Hiroshi Noji
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at
 
   http://www.apache.org/licenses/LICENSE-2.0
   
 Unless required by applicable low or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS.
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the Licensefor the specific language governing permissions and
 limitations under the License.
*/
import java.util.Properties
import jigg.ml.keras.KerasParser

import scala.xml.{Node, NodeSeq}
import jigg.util.XMLUtil.RichNode

abstract class BunsetsuKerasAnnotator(override val name: String, override val props: Properties) extends ExternalProcessSentencesAnnotator { self =>

  def defaultModelFileName = "bunsetsu_model.h5"
  def defaultTableFileName = "table.json"

  @Prop(gloss = "Model file (if omitted, the default path is used to search file)") var model = ""
  @Prop(gloss = "Lookup table for mapping character into id space") var table = ""

  readProps()

  localAnnotators // instantiate lazy val here

  override def description =s"""${super.description}

  Annotate chunks (bunsetsu).

  Note about dictionary:
    Distionary settings of tokenizer (e.g., mecab) should be IPA.

"""

  trait LocalBunsetsuKerasAnntoator extends LocalAnnotator {
    lazy val bunsetsuSplitter: QueueBunsetsuSplitter = new QueueBunsetsuSplitter

    override def init(): Unit = {
      bunsetsuSplitter
    }

    override def newSentenceAnnotation(sentence: Node): Node = {
      def tid (sindex: String, tindex: Int) = sindex + "_" + tindex

      val sindex = (sentence \ "@id").toString

      var tokenIndex = 0
      val chunks = bunsetsuSplitter.b.parsing(sentence)

      val tokenNodes = boundaries.map{t =>
        val node = tokenToNode(text.slice(t._1, t._2), t, tid(sindex, tokenIndex))
        tokenIndex += 1
        node
      }

      sentence addChild <tokens annotators={ name }>{ tokenNodes }</tokens>
    }

    private def tokenToNode(form: String, span:(Int, Int), id: String) =
        <token
        id={ id }
        form={ form }
        offsetBegin = { span._1.toString }
        offsetEnd = { span._2.toString }
        />

    class QueueBunsetsuSplitter {
      private def makeBunsetsuSplitter: KerasParser = model match {
        case "" =>
          System.err.println(s"No model file is given. Try to search default path: $defaultModelFileName")
          table match {
            case "" =>
              System.err.println(s"No lookup table file is given. Try to search default path: $defaultTableFileName")
              KerasParser(defaultModelFileName, defaultTableFileName)
            case tableFile =>
              KerasParser(defaultTableFileName, tableFile)
          }
        case modelFile =>
          table match {
            case "" =>
              System.err.println(s"No lookup table file is given. Try to search default path: $defaultTableFileName")
              KerasParser(model, defaultTableFileName)
            case tableFile =>
              KerasParser(model, tableFile)
          }
      }

      val b: KerasParser = makeBunsetsuSplitter
    }

  }

  override def requirementsSatisfied(): Set[Requirement] = Set(JaRequirement.BunsetsuChunk)
}

class IPABunsetsuKerasAnnotator(name: String, props: Properties) extends BunsetsuKerasAnnotator(name, props){

  def mkLocalAnnotator = new IPALocalBunsetsuKerasAnnotator

  class IPALocalBunsetsuKerasAnnotator extends LocalBunsetsuKerasAnntoator {
    val featAttributes = Array("lemma").map("@"+_)
  }

  override def requires() = Set(JaRequirement.TokenizeWithIPA)
}

object BunsetsuKerasAnnotator extends AnnotatorCompanion[BunsetsuKerasAnnotator] {

  val model = ""
  val table = ""
}
