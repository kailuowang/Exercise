package com.kailuowang.naturalLanguage.naive

import com.kailuowang.naturalLanguage._
import scala.annotation.tailrec

object TokenParser {
  val normalWordPattern = """\w+""".r
  val acronymPattern = """(?:\w\.){2,}""".r
  val normalPunctuationPatten = """\p{Punct}""".r
  val ellipsisPatten = """\.{3}""".r

  val wordPatterns = List(normalWordPattern, acronymPattern)
  val punctPatterns = List(normalPunctuationPatten, ellipsisPatten)

  type Parser =  PartialFunction[String, Token]

  def parse: Function[String, Option[Token]] = (
      wordPatterns.map { p =>
        { case w @ p() => Word(w) } : Parser
      } ++
      punctPatterns.map { p =>
        { case punct @ p() => Punctuation(punct) } : Parser
      }
    ).reduce(_ orElse _).lift

  def parseTokens(string: String): Vector[Token] = {
    @tailrec
    def loop(string: String, memo: Vector[Token]): Vector[Token] =
      if(string.isEmpty)
        memo
      else{
        findLongestToken(string) match {
          case (Some(token), rest) => loop(rest, memo :+ token)
          case (None, _) => memo
        }
      }
    loop(string, Vector.empty)
  }

  @tailrec
  private[naive] def findLongestToken(string: String, rest: String = "" ): (Option[Token], String) = {
    if(string.isEmpty)
      (None, rest)
    else {
      val tokenAttempt = TokenParser.parse(string.mkString)
      if(tokenAttempt.isDefined)
        (tokenAttempt, rest)
      else
        findLongestToken(string.init, string.last + rest)
    }
  }

}
