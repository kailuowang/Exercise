package com.kailuowang.naturalLanguage

import org.specs2.mutable.Specification
import org.specs2.specification.Scope
import com.kailuowang.concordance.DataTypes.SentenceInTokens


class NaiveProcessorSpec extends Specification {


  "tokenize" should {
    "find regular words as token" in new TokenizeContext {
      tokens must containAllOf(Seq(Word("write"), Word("alphabetical"), Word("program"), Word("will"), Word("that")))
    }

    "Get first word as token" in new TokenizeContext {
      tokens.head must equalTo(Word("Given"))
    }

    "Get last word as token" in new TokenizeContext {
      tokens.filter(_.isInstanceOf[Word]).last must equalTo(Word("appeared"))
    }

    "Get words with punctuation attached" in new TokenizeContext {
      tokens must containAllOf(Seq(Word("English"), Word("concordance"), Word("frequencies"), Word("occurrences"), Word("Bonus")))
    }

    "Get last token" in new TokenizeContext {
      tokens.last must equalTo(Punctuation("."))
    }

    "Get abbreviation as word token" in new TokenizeContext {
      tokens must contain(Word("i.e."))
    }

    "Get punctuation as punctuation token" in new TokenizeContext {
      tokens must containAllOf(Seq(Punctuation(","), Punctuation("."), Punctuation(":")))
    }

    "Ignore line-breakers" in new TokenizeContext {
      tokens.exists(_.value.contains("\n")) must beFalse
    }

    "find words before and after line breaker" in new TokenizeContext {
      tokens must containAllOf(Seq(Word("concordance"), Word("Bonus")))
    }

    trait TokenizeContext extends Scope {
      val input = "Given an arbitrary text document written in English, write a program that will generate a \n\nconcordance, i.e. an alphabetical list of all word occurrences, labeled with word frequencies. \n\nBonus: label each word with the sentence numbers in which each occurrence appeared.".toStream
      lazy val tokens = NaiveProcessor.tokenize(input)
    }
  }

  "breakToSentences" should {
    def mkString(sit: SentenceInTokens) = sit.map{
      case Word(value) => " " + value
      case Punctuation(value) => value
    }.mkString.stripPrefix(" ")

    "find first sentence" in {
      val input = NaiveProcessor.tokenize("This is a test. And there is another sentence.".toStream)
      val sentences = NaiveProcessor.splitToSentences(input).map(mkString(_))
      sentences.head must equalTo("This is a test.")
    }

    "find last sentence" in {
      val input =  NaiveProcessor.tokenize("This is a test. And there is another sentence.".toStream)
      val sentences = NaiveProcessor.splitToSentences(input).map(mkString(_))
      sentences must contain("And there is another sentence.")
    }

    "find sentence in the middle" in {
      val input =  NaiveProcessor.tokenize("This is a test. A middle sentence starts here! And there is another sentence.".toStream)
      val sentences = NaiveProcessor.splitToSentences(input).map(mkString(_))
      sentences must contain("A middle sentence starts here!")
    }

    "works with one character" in {
      val input =  NaiveProcessor.tokenize("a".toStream)
      NaiveProcessor.splitToSentences(input).map(mkString(_)) must equalTo(Stream("a"))
    }

    "works with given example" in {
      val input = NaiveProcessor.tokenize("Given an arbitrary text document written in English, write a program that will generate a \n\nconcordance, i.e. an alphabetical list of all word occurrences, labeled with word frequencies. \n\nBonus: label each word with the sentence numbers in which each occurrence appeared.".toStream)
      val sentences = NaiveProcessor.splitToSentences(input).map(mkString(_))
      sentences.size must equalTo(2)
      sentences.head must equalTo("Given an arbitrary text document written in English, write a program that will generate a concordance, i.e. an alphabetical list of all word occurrences, labeled with word frequencies.")
      sentences.last must equalTo("Bonus: label each word with the sentence numbers in which each occurrence appeared.")
    }

  }
}
