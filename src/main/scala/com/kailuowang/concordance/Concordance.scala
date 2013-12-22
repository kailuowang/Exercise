package com.kailuowang.concordance
import DataTypes._
import com.kailuowang.naturalLanguage.{Word, Processor}
import scala.collection.immutable.TreeMap


object Concordance {
  val emptyOccurrence = (0, List[Int]())
  def get(input: Stream[Char])(implicit lp: Processor): Map[String, Occurrences] = {
    val sentencesInTokens = lp.splitToSentences(lp.tokenize(input))
    val sentences: Stream[Sentence] = sentencesInTokens zip Stream.from(1)
    sentences.foldLeft(TreeMap[String, Occurrences]()){ (memo, sentence ) =>
      val (tokens, sentenceNum) = sentence
      tokens.foldLeft(memo) {
        case (innerMemo, Word(word)) => {
          val (numOfOccurrences, occurrencesInSentences) = innerMemo.get(word.toLowerCase).getOrElse(emptyOccurrence)
          innerMemo + (word.toLowerCase -> (numOfOccurrences + 1, sentenceNum :: occurrencesInSentences))
        }
        case (innerMemo, _) => innerMemo
      }
    }.map {
      case (word, (numOfOccurrences, occurrencesInSentence)) => (word, (numOfOccurrences, occurrencesInSentence.reverse))
    }
  }
}
