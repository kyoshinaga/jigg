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

  @Prop(gloss = "What kind of JukaiNLP do you use? Currentrly supported:(Tokenizer)") var mode = JukaiNLPAnnotator.defaultMode
  @Prop(gloss = "Model file") var  modelFileName = ""
  @Prop(gloss = "Dictionary type") var dictionary = "juman"

  readProps()

  lazy val tokenizer: QueueTokenizer = new QueueTokenizer

  override def description: String = {
    s"""${super.description}

  A wrapper for JukaiNLP.

  The path to the model file can be changed by setting -${name}.modelFileName.

     """
  }

  override def init() = {
    tokenizer
  }

  override def newSentenceAnnotation(sentence: Node): Node = {
    def tid (sindex: String, tindex: Int) = sindex + "_" + tindex

    val sindex = (sentence \ "@id").toString
    val text = sentence.text

    var tokenIndex = 0
    val tokenWithRanges = tokenizer.t.tokenizeWithRanges(text)

    val tokenNodes = tokenWithRanges.map { t =>
      val node = tokenToNode(t._1, (t._2._1.toString,t._2._2.toString), tid(sindex, tokenIndex))
      tokenIndex += 1
      node
    }

    val tokenAnnotation = <tokens annotators={ name }>{ tokenNodes }</tokens>

    jigg.util.XMLUtil.addChild(sentence, tokenAnnotation)
  }

  protected def tokenToNode( form: String, span: (String, String), id: String): Node

  class QueueTokenizer {

    private def mkTokenizer: Tokenizer = modelFileName match {
      case "" =>
        System.err.println(s"No model file is given. Try to search from default path: ${defaultModelFile}")
        Tokenizer(defaultModelFile)
      case _ =>
        Tokenizer(modelFileName)
    }

    val t = mkTokenizer
  }

}

class TokenizerJukaiNLPJuman(name: String, props: Properties)
  extends JukaiNLPAnnotator(name, props){

  def tokenToNode(form: String, span: (String, String), id: String) =
    <token
      id={ id }
      form={ form }
      offsetBegin={ span._1 }
      offsetEnd={ span._2 }/>

  override def requires =Set(Requirement.Ssplit)
  override def requirementsSatisfied(): Set[Requirement] = Set(JaRequirement.TokenizeWithJumandic)

}

class TokenizerJukaiNLPUnidic(name: String, props: Properties)
  extends JukaiNLPAnnotator(name, props){
  def tokenToNode(form: String, span: (String, String), id: String) =
    <token
    id={ id }
    form={ form }
    offsetBegin={ span._1 }
    offsetEnd={ span._2 }
    />
  override def requires = Set(Requirement.Ssplit)

  override def requirementsSatisfied(): Set[Requirement] = Set(JaRequirement.TokenizeWithUnidic)
}

/*
class TokenizerJukaiNLPLUW(name: String, props: Properties) extends JukaiNLPAnnotator(name, props) {
  override def requires = Set(JaRequirement.TokenizeWithUnidic)

  override def requirementsSatisfied(): Set[Requirement] = Set(JaRequirement.TokenizeWithUnidic)
}
*/

object JukaiNLPAnnotator extends AnnotatorCompanion[JukaiNLPAnnotator] {

  def defaultMode = "Tokenizer"
  def defaultDict = "juman"

  override def fromProps(name: String, props: Properties) = {
    val mode = name + ".mode"
    val dict = name + ".dictionary"

    PropertiesUtil.findProperty(mode, props) getOrElse (defaultMode + "") match {
      case "Tokenizer" =>
        PropertiesUtil.findProperty(dict, props) getOrElse (defaultDict + "") match {
          case "juman" => new TokenizerJukaiNLPJuman(name, props)
          case "unidic" => new TokenizerJukaiNLPUnidic(name, props)
        }
//      case "LUW" =>
//        throw("not yet supported")
    }
  }
}
