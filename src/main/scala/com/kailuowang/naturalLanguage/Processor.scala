package com.kailuowang.naturalLanguage

trait Processor {
  def splitToSentences(input: Stream[Token]): Stream[Vector[Token]]

  def tokenize(input: Stream[Char]): Stream[Token]
}





