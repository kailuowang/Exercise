package com.kailuowang.concordance

import com.kailuowang.naturalLanguage.Token

object DataTypes {
  type Occurrences = (Int, List[Int])
  type SentenceInTokens = Stream[Token] //(Tokens in the sentence, Sentence #)
  type Sentence = (SentenceInTokens, Int) //(Tokens in the sentence, Sentence #)
}
