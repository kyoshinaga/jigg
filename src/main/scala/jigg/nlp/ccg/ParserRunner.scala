package jigg.nlp.ccg

/*
 Copyright 2013-2016 Hiroshi Noji

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

import lexicon._
import parser.{ActionLabel, KBestDecoder}
import jigg.ml.FixedPerceptron

import breeze.config.{CommandLineParser, Help}

import scala.collection.mutable.{ArraySeq}

import java.io.File


class ParserRunner(model: ParserModel, params: ParserRunner.Params) {

  val tagger = new SuperTaggerRunner(model.taggerModel, params.tagger)
  val perceptron = new FixedPerceptron[ActionLabel](model.weights)
  val decoder = model.mkDecoder(params.beam, perceptron)

  val preferConnected = params.preferConnected

  def decode[S<:TaggedSentence](sentences: Array[S]): Array[Derivation] = {

    val predDerivations = sentences.zipWithIndex map {
      case (sentence, i) =>
        if (i % 100 == 0)
          System.err.print(i + "\t/" + sentences.size + " have been processed.\r")
        decodeOne(sentence)
    }
    System.err.println()
    predDerivations
  }

  def decodeOne[S<:TaggedSentence](sentence: S): Derivation =
    kBestDerivations(sentence, 1)(0)._1

  def kBestDerivations[S<:TaggedSentence](sentence: S, k: Int)
      : Seq[(Derivation, Double)] = {
    val superTaggedSentence = tagger.assignKBest(sentence)

    decoder match {
      case decoder: KBestDecoder =>
        decoder predictKbest (k, superTaggedSentence, preferConnected)
      case decoder =>
        Seq(decoder predict superTaggedSentence)
    }
  }
}

object ParserRunner {

  @Help(text="Params for testing/evaluating parser")
  case class Params(
    @Help(text="Beam size") beam: Int = 32,
    @Help(text="Prefer connected derivation at prediction") preferConnected: Boolean = true,
    tagger: SuperTaggerRunner.Params = new SuperTaggerRunner.Params()
  )
}
