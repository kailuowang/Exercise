package com.kailuowang.naturalLanguage

import com.kailuowang.concordance.DataTypes._
import scala.collection.immutable.Stream._
import scala.annotation.tailrec

trait Processor {
  def breakToSentences(input: Stream[Token]) : Stream[SentenceInTokens]
  def tokenize(input: Stream[Char]): Stream[Token]
}

object NaiveProcessor extends Processor {

  //a simple helper method from http://stackoverflow.com/questions/4636610/regular-expression-and-pattern-matching-in-scala
  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  def tokenize(input: Stream[Char]): Stream[Token] = {
    if(input.isEmpty)
      Empty
    else {
      val (token, rest) = parseStartTokens(input)
      token.toStream ++ tokenize(rest)
    }
  }


  def parseStartTokens(input: Stream[Char]): (Seq[Token], Stream[Char]) = {
    @tailrec
    def loop(tokenString: String, toParse: Stream[Char]): (Seq[Token], Stream[Char]) =
      toParse match {
        case start #:: ' ' #:: tail => (parse(tokenString + start), tail)
        case start #:: Empty => (parse(tokenString + start), Empty)
        case start #:: tail => loop(tokenString + start, tail)
      }

    loop("", input.dropWhile(_ == ' '))
  }

  def parse(tokenString: String): Seq[Token] = tokenString.replace("\n", "") match {
    case r"\w+" => Seq(Word(tokenString))
    case r"(?:\w\.){2,}" => Seq(Word(tokenString))
    case r"(\w+)${word}(\W)${punc}" => Seq(Word(word), Punctuation(punc))
  }


  def breakToSentences(input: Stream[Token]): Stream[SentenceInTokens] = {
    if(input.isEmpty)
      Empty
    else sliceByFirstSentence(input) match {
      case (sentence, rest) => sentence #:: breakToSentences(rest)
    }
  }

  def sliceByFirstSentence(input: Stream[Token]): (SentenceInTokens, Stream[Token]) = {
    @tailrec
    def loop(sentence: SentenceInTokens, rest: Stream[Token]): (SentenceInTokens, Stream[Token]) =
      rest match {
        case head #:: Empty  => (sentence ++ Stream(head), Empty)
        case (p: Punctuation) #:: tail if p.endsSentence => (sentence ++ Stream(p), tail)
        case head #:: tail => loop(sentence ++ Stream(head), tail)
      }

    loop(empty, input)
  }

  implicit class PunctuationOps(p: Punctuation) {

    def endsSentence: Boolean = List(".", "!", "?", "...").contains(p.value)
  }
}
