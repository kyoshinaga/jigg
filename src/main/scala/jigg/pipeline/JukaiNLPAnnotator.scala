package jigg.pipeline

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

import java.util
import java.util.Properties

import jigg.ml.merlin.Tokenizer
import jigg.util.{Prop, PropertiesUtil}

import scala.annotation.tailrec
import scala.sys.Prop
import scala.xml.Node


abstract class JukaiNLPAnnotator(override val name: String, override val props: Properties)
  extends SentencesAnnotator {

  val defaultModelFile = "model.h5"

  @Prop(gloss = "What kind of JukaiNLP do you use? Currentrly supported:Tokenizer [Tokenizer]") var mode = JukaiNLPAnnotator.defaultMode
  @Prop(gloss = "Model file") var  modelFileName = ""

  readProps()

  val jukaiTokenizer: Tokenizer = modelFileName match {
    case "" =>
      System.err.println(s"No model file is given. Try to search from default path: ${defaultModelFile}")
      Tokenizer(defaultModelFile)
    case _ =>
      Tokenizer(modelFileName)
  }

  override def description: String = {
    def keyName = makeFullName("command")

    s"""${super.description}

  Tokenize sentence by JukaiNLP.
  Current mode is ${mode}

     """
  }

  override def newSentenceAnnotation(sentence: Node): Node = {
    def tid (sindex: String, tindex: Int) = sindex + "_" + tindex

    val sindex = (sentence \ "@id").toString
    val text = sentence.text

    var tokenIndex = 0
    val tokenWithRanges = jukaiTokenizer.tokenizeWithRanges(text)

    val tokenNodes = tokenWithRanges.map { t =>
      val node = tokenToNode(t._1, (t._2._1.toString,t._2._2.toString), tid(sindex, tokenIndex))
      tokenIndex += 1
      node
    }

    val tokenAnnotation = <tokens annotators={ name }>{ tokenNodes }</tokens>

    jigg.util.XMLUtil.addChild(sentence, tokenAnnotation)
  }

  protected def tokenToNode( form: String, span: (String, String), id: String): Node

  override def requires =Set(Requirement.Ssplit)

}

class TokenizerJukaiNLP(name: String, props: Properties)
  extends JukaiNLPAnnotator(name, props){

  def tokenToNode(form: String, span: (String, String), id: String) =
    <token
      id={ id }
      form={ form }
      offsetBegin={ span._1 }
      offsetEnd={ span._2 }/>

  override def requirementsSatisfied(): Set[Requirement] = Set(JaRequirement.TokenizeWithJumandic)

}

object JukaiNLPAnnotator extends AnnotatorCompanion[JukaiNLPAnnotator] {

  def defaultMode = "Tokenizer"

  override def fromProps(name: String, props: Properties) = {
    val key = name + ".mode"
    PropertiesUtil.findProperty(key, props) getOrElse (defaultMode+"") match {
      case "Tokenizer" => new TokenizerJukaiNLP(name, props)
    }
  }
}
