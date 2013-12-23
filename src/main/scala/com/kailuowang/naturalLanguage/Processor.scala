package com.kailuowang.naturalLanguage

import scalaz.EphemeralStream


trait Processor {
  def splitToSentences(input: EphemeralStream[Token]): EphemeralStream[Vector[Token]]

  def tokenize(input: EphemeralStream[Char]): EphemeralStream[Token]
}





