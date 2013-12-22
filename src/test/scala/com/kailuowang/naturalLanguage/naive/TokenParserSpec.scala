package com.kailuowang.naturalLanguage.naive

import org.specs2.mutable.Specification
import com.kailuowang.naturalLanguage.{Punctuation, Word}

class TokenParserSpec extends Specification {
  "parse" should {
    "parse normal word" in {
      TokenParser.parse("apple") must beSome(Word("apple"))
    }

    "parse acronym word" in {
      TokenParser.parse("i.e.") must beSome(Word("i.e."))
    }

    "parse normal punctuation" in {
      TokenParser.parse(".") must beSome(Punctuation("."))
    }

    "parse ellipsis punctuation" in {
      TokenParser.parse("...") must beSome(Punctuation("..."))
    }

    "not parse word with space" in {
      TokenParser.parse("apple ") must beNone
    }

    "not parse punctuation with space" in {
      TokenParser.parse(". ") must beNone
    }

    "not parse word with punctuation" in {
      TokenParser.parse("apple.") must beNone
    }
  }

  "findLongestToken" should {
    "find the a single word" in {
      val (token, rest) = TokenParser.findLongestToken("apple")
      token must beSome(Word("apple"))
    }

    "find in word with punctuation" in {
      val (token, rest) = TokenParser.findLongestToken("apple.")
      token must beSome(Word("apple"))
      rest must equalTo(".")
    }

    "find acronym" in {
      val (token, _) = TokenParser.findLongestToken("i.e.")
      token must beSome(Word("i.e."))
    }
  }

  "parseTokens" should {

    "find the a single word" in {
      TokenParser.parseTokens("apple") must equalTo(Vector(Word("apple")))
    }

    "find the word and punctuation" in {
      TokenParser.parseTokens("apple.") must equalTo(Vector(Word("apple"), Punctuation(".")))
    }

    "find the words with dash between them" in {
      TokenParser.parseTokens("apple-orange") must equalTo(Vector(Word("apple"), Punctuation("-"), Word("orange")))
    }

    "find multiple punctuations" in {
      TokenParser.parseTokens("\"Really?\"") must equalTo(Vector(Punctuation("\""), Word("Really"), Punctuation("?"), Punctuation("\"")))
    }

  }

}
