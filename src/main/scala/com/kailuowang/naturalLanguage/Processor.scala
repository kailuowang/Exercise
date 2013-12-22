package com.kailuowang.naturalLanguage

import com.kailuowang.concordance.DataTypes._
import scala.collection.immutable.Stream._
import scala.annotation.tailrec

trait Processor {
  def splitToSentences(input: Stream[Token]) : Stream[SentenceInTokens]
  def tokenize(input: Stream[Char]): Stream[Token]
}

object NaiveProcessor extends Processor {

  def tokenize(input: Stream[Char]): Stream[Token] = {
    if(input.isEmpty)
      Empty
    else {
      val (tokens, rest) = parseStartTokens(input)
      tokens.toStream ++ tokenize(rest)
    }
  }

  def splitToSentences(input: Stream[Token]): Stream[SentenceInTokens] = {
    if(input.isEmpty)
      Empty
    else {
      val (sentence, rest) = sliceByFirstSentence(input)
      sentence #:: splitToSentences(rest)
    }
  }

  private def parseStartTokens(input: Stream[Char]): (Seq[Token], Stream[Char]) = {
    @tailrec
    def loop(tokenString: String, toParse: Stream[Char]): (Seq[Token], Stream[Char]) =
      toParse match {
        case start #:: ' ' #:: tail => (parse(tokenString + start), tail)
        case start #:: Empty => (parse(tokenString + start), Empty)
        case start #:: tail => loop(tokenString + start, tail)
      }

    loop("", input.dropWhile(_ == ' '))
  }

  private def parse(tokenString: String): Seq[Token] = tokenString.replace("\n", "") match {
    case r"\w+" => Seq(Word(tokenString))
    case r"(?:\w\.){2,}" => Seq(Word(tokenString))
    case r"(\w+)${word}(\W)${punc}" => Seq(Word(word), Punctuation(punc))
  }


  private def sliceByFirstSentence(input: Stream[Token]): (SentenceInTokens, Stream[Token]) = {
    @tailrec
    def loop(sentence: SentenceInTokens, rest: Stream[Token]): (SentenceInTokens, Stream[Token]) =
      rest match {
        case head #:: Empty  => (sentence ++ Stream(head), Empty)
        case (p: Punctuation) #:: tail if p.endsSentence => (sentence ++ Stream(p), tail)
        case head #:: tail => loop(sentence ++ Stream(head), tail)
      }

    loop(empty, input)
  }

  private implicit class PunctuationOps(p: Punctuation) {
    def endsSentence: Boolean = List(".", "!", "?", "...").contains(p.value)
  }

  //a simple helper method from http://stackoverflow.com/questions/4636610/regular-expression-and-pattern-matching-in-scala
  private implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

}
