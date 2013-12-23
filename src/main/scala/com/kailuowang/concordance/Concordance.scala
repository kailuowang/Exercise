package com.kailuowang.concordance

import com.kailuowang.naturalLanguage._
import scala.collection.immutable.TreeMap
import scalaz.EphemeralStream
import scalaz.EphemeralStream._


object Concordance {
  type SentenceLabel = Int
  type SentenceLabels = Vector[SentenceLabel]
  type Sentence = (Vector[Token], SentenceLabel) //(Tokens in the sentence, Sentence #)
  type Occurrences = (Int, SentenceLabels)
  val emptyOccurrence = (0, Vector[SentenceLabel]())

  def get(input: Iterator[Char])(implicit lp: Processor): List[(String, Occurrences)] = {
    def toStream[A](iter: Iterator[A]): EphemeralStream[A] =
      if (iter.hasNext) iter.next ##:: toStream(iter)
      else emptyEphemeralStream[A]
    get(toStream(input))
  }

  def get(input: EphemeralStream[Char])(implicit lp: Processor): List[(String, Occurrences)] = {
    val sequences = EphemeralStream.fromStream(Stream.from(1))
    val sentences: EphemeralStream[Sentence] = lp.splitToSentences(lp.tokenize(input)) zip sequences

    sentences.foldLeft(TreeMap[String, Occurrences]()) {  (memo) => (sentence) => {
      val (sentenceTokens, sentenceLabel) = sentence
      sentenceTokens.foldLeft(memo) {
        case (innerMemo, Word(tokenValue)) => {
          val word = tokenValue.toLowerCase
          val (numOfOccurrences, sentenceLabels) = innerMemo.get(word).getOrElse(emptyOccurrence)
          innerMemo + (word -> (numOfOccurrences + 1, sentenceLabels :+ sentenceLabel))
        }
        case (innerMemo, _) => innerMemo   //ignore non-word tokens
      }
    }}.toList
  }
}
