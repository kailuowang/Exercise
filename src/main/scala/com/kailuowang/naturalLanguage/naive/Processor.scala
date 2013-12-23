package com.kailuowang.naturalLanguage.naive

import com.kailuowang.naturalLanguage._
import scala.annotation.tailrec
import scala.collection.immutable.Stream.Empty

object Processor extends Processor {

  def tokenize(input: Stream[Char]): Stream[Token] = {
    if (input.isEmpty)
      Empty
    else {
      val (tokens, rest) = sliceByFirstTokens(input)
      tokens.toStream ++ tokenize(rest)
    }
  }

  def splitToSentences(input: Stream[Token]): Stream[Vector[Token]] = {
    if (input.isEmpty)
      Empty
    else {
      val (sentence, rest) = sliceByFirstSentence(input)
      sentence #:: splitToSentences(rest)
    }
  }

  private[naive] def sliceByFirstTokens(input: Stream[Char]): (Vector[Token], Stream[Char]) = {
    def isTokenBreak(c: Char) = c == ' ' || c == '\n'
    @tailrec
    def loop(tokenString: String, rest: Stream[Char]): (Vector[Token], Stream[Char]) =
      rest match {
        case Empty => (TokenParser.parseTokens(tokenString), Empty)
        case head #:: break #:: rest if isTokenBreak(break) => (TokenParser.parseTokens(tokenString + head), rest)
        case head #:: rest => loop(tokenString + head, rest)
      }

    loop("", input.dropWhile(isTokenBreak(_)))
  }


  private def sliceByFirstSentence(input: Stream[Token]): (Vector[Token], Stream[Token]) = {
    @tailrec
    def loop(sentence: Vector[Token], rest: Stream[Token]): (Vector[Token], Stream[Token]) =
      rest match {
        case head #:: Empty => (sentence :+ head, Empty)
        case (p: Punctuation) #:: tail if p.endsSentence => (sentence :+ p, tail)
        case head #:: tail => loop(sentence :+ head, tail)
      }

    loop(Vector.empty, input)
  }

  private implicit class PunctuationOps(p: Punctuation) {
    def endsSentence: Boolean = List(".", "!", "?", "...").contains(p.value)
  }

}
