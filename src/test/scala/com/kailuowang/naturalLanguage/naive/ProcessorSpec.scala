package com.kailuowang.naturalLanguage.naive

import com.kailuowang.naturalLanguage._
import org.specs2.mutable.Specification
import scalaz.EphemeralStream


class ProcessorSpec extends Specification {
  implicit def toStream(str: String) = EphemeralStream(str: _*)

  "sliceByFirstToken" should {
    "find normal word" in {
      val (token, rest) = Processor.sliceByFirstTokens("apple is big")
      token must equalTo(Vector(Word("apple")))
      rest.mkString must equalTo("is big")
    }

    "find word after space" in {
      val (token, _) = Processor.sliceByFirstTokens(" is big")
      token must equalTo(Vector(Word("is")))
    }

    "find word after line-breaker" in {
      val (token, _) = Processor.sliceByFirstTokens("\n\nis big")
      token must equalTo(Vector(Word("is")))
    }

    "find word and punctuation" in {
      val (token, rest) = Processor.sliceByFirstTokens("apple. Orange")
      token must equalTo(Vector(Word("apple"), Punctuation(".")))
      rest.mkString must equalTo("Orange")
    }

    "find the only word" in {
      val (token, rest) = Processor.sliceByFirstTokens("apple")
      token must equalTo(Vector(Word("apple")))
      rest must beEmpty
    }

    "find the only punctuation" in {
      val (token, rest) = Processor.sliceByFirstTokens(".")
      token must equalTo(Vector(Punctuation(".")))
      rest must beEmpty
    }
  }


  "tokenize" should {
    "return empty stream when string is emtpy" in {
      Processor.tokenize("") must beEmpty
    }

    "find words and punctuation as tokens" in {
      val input = "Write a program that \n\ngenerates a concordance, i.e. an alphabetical list."
      Processor.tokenize(input).toList must equalTo(List(
        Word("Write"),
        Word("a"),
        Word("program"),
        Word("that"),
        Word("generates"),
        Word("a"),
        Word("concordance"),
        Punctuation(","),
        Word("i.e."),
        Word("an"),
        Word("alphabetical"),
        Word("list"),
        Punctuation(".")
      ))
    }
  }

  "breakToSentences" should {
    def mkString(sit: Vector[Token]) = sit.map{
      case Word(value) => " " + value
      case Punctuation(value) => value
    }.mkString.stripPrefix(" ")

    "find first sentence" in {
      val input = Processor.tokenize("This is a test. And there is another sentence.")
      val sentences = Processor.splitToSentences(input).map(mkString(_))
      sentences.head() must equalTo("This is a test.")
    }

    "find last sentence" in {
      val input =  Processor.tokenize("This is a test. And there is another sentence.")
      val sentences = Processor.splitToSentences(input).map(mkString(_)).toList
      sentences must contain("And there is another sentence.")
    }

    "find sentence in the middle" in {
      val input =  Processor.tokenize("This is a test. A middle sentence starts here! And there is another sentence.")
      val sentences = Processor.splitToSentences(input).map(mkString(_)).toList
      sentences must contain("A middle sentence starts here!")
    }

    "works with one character" in {
      val input =  Processor.tokenize("a")
      Processor.splitToSentences(input).map(mkString(_)).size must equalTo(1)
      Processor.splitToSentences(input).map(mkString(_)).head() must equalTo("a")
    }

    "works with given example" in {
      val input = Processor.tokenize("Given an arbitrary text document written in English, write a program that will generate a \n\nconcordance, i.e. an alphabetical list of all word occurrences, labeled with word frequencies. \n\nBonus: label each word with the sentence numbers in which each occurrence appeared.")
      val sentences = Processor.splitToSentences(input).map(mkString(_))
      sentences.size must equalTo(2)
      sentences.head() must equalTo("Given an arbitrary text document written in English, write a program that will generate a concordance, i.e. an alphabetical list of all word occurrences, labeled with word frequencies.")
      sentences.last must equalTo("Bonus: label each word with the sentence numbers in which each occurrence appeared.")
    }

    "Find sentences in quotes" in {
      val input = Processor.tokenize("I said, \"Apple is tasty.\" He disagrees with me. But I am okay with that.")
      val sentences = Processor.splitToSentences(input).toList
      sentences.size must equalTo(3)
      sentences(0) must contain(Word("tasty"))
      sentences(1) must contain(Word("He"))
    }

  }
}
