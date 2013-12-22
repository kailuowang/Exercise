package com.kailuowang.concordance

import com.kailuowang.naturalLanguage.Token

object DataTypes {
  type SentenceLabel = Int
  type SentenceLabels = Vector[SentenceLabel]
  type Occurrences = (Int, SentenceLabels)
  type SentenceInTokens = Stream[Token] //(Tokens in the sentence, Sentence #)
  type Sentence = (SentenceInTokens, SentenceLabel) //(Tokens in the sentence, Sentence #)

  val emptyOccurrence = (0, Vector[SentenceLabel]())

}
