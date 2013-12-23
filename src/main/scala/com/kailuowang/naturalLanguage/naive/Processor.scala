package com.kailuowang.naturalLanguage.naive

import com.kailuowang.naturalLanguage._
import scala.annotation.tailrec
import scalaz.EphemeralStream
import scalaz.EphemeralStream._

object Processor extends Processor {
  val emptyChars = emptyEphemeralStream[Char]
  val emptyTokens = emptyEphemeralStream[Token]
  def tokenize(input: EphemeralStream[Char]): EphemeralStream[Token] = {
    if (input.isEmpty)
      emptyTokens
    else {
      val (tokens, rest) = sliceByFirstTokens(input)
      EphemeralStream(tokens: _*) ++ tokenize(rest)
    }
  }

  def splitToSentences(input: EphemeralStream[Token]): EphemeralStream[Vector[Token]] = {
    if (input.isEmpty)
      emptyEphemeralStream
    else {
      val (sentence, rest) = sliceByFirstSentence(input)
      sentence ##:: splitToSentences(rest)
    }
  }

  private[naive] def sliceByFirstTokens(input: EphemeralStream[Char]): (Vector[Token], EphemeralStream[Char]) = {
    def isTokenBreak(c: Char) = c == ' ' || c == '\n'
    @tailrec
    def loop(tokenString: String, rest: EphemeralStream[Char]): (Vector[Token], EphemeralStream[Char]) =
      rest match {
        case e if e.isEmpty => (TokenParser.parseTokens(tokenString), emptyChars)
        case head ##:: break ##:: rest if isTokenBreak(break) => (TokenParser.parseTokens(tokenString + head), rest)
        case head ##:: rest => loop(tokenString + head, rest)
      }

    loop("", input.dropWhile(isTokenBreak(_)))
  }


  private def sliceByFirstSentence(input: EphemeralStream[Token]): (Vector[Token], EphemeralStream[Token]) = {
    @tailrec
    def loop(sentence: Vector[Token], rest: EphemeralStream[Token]): (Vector[Token], EphemeralStream[Token]) =
      rest match {
        case head ##:: tail if tail.isEmpty => (sentence :+ head, emptyTokens)
        case (p: Punctuation) ##:: tail if p.endsSentence => (sentence :+ p, tail)
        case head ##:: tail => loop(sentence :+ head, tail)
      }

    loop(Vector.empty, input)
  }

  private implicit class PunctuationOps(p: Punctuation) {
    def endsSentence: Boolean = List(".", "!", "?", "...").contains(p.value)
  }

}
