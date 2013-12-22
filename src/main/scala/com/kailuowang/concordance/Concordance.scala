package com.kailuowang.concordance

import com.kailuowang.naturalLanguage.{Token, Word, Processor}
import scala.collection.immutable.TreeMap


object Concordance {
  type SentenceLabel = Int
  type SentenceLabels = Vector[SentenceLabel]
  type Sentence = (Vector[Token], SentenceLabel) //(Tokens in the sentence, Sentence #)
  type Occurrences = (Int, SentenceLabels)
  val emptyOccurrence = (0, Vector[SentenceLabel]())

  def get(input: Stream[Char])(implicit lp: Processor): List[(String, Occurrences)] = {

    val sentences: Stream[Sentence] = lp.splitToSentences(lp.tokenize(input)) zip Stream.from(1)

    sentences.foldLeft(TreeMap[String, Occurrences]()) { case (memo, (tokens, sentenceLabel)) =>
      tokens.foldLeft(memo) {
        case (innerMemo, Word(tokenValue)) => {
          val word = tokenValue.toLowerCase
          val (numOfOccurrences, sentenceLabels) = innerMemo.get(word).getOrElse(emptyOccurrence)
          innerMemo + (word -> (numOfOccurrences + 1, sentenceLabels :+ sentenceLabel))
        }
        case (innerMemo, _) => innerMemo   //ignore non-word tokens
      }
    }.toList
  }
}
