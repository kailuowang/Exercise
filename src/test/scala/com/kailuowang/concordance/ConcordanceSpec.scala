package com.kailuowang.concordance

import org.specs2.mutable.Specification
import com.kailuowang.naturalLanguage.naive.Processor

class ConcordanceSpec extends Specification {
  "Get" should {
    "work with the given example" in {
      val expectedResult = List(
        "a" ->(2, Vector(1, 1)),
        "all" ->(1, Vector(1)),
        "alphabetical" ->(1, Vector(1)),
        "an" ->(2, Vector(1, 1)),
        "appeared" ->(1, Vector(2)),
        "arbitrary" ->(1, Vector(1)),
        "bonus" ->(1, Vector(2)),
        "concordance" ->(1, Vector(1)),
        "document" ->(1, Vector(1)),
        "each" ->(2, Vector(2, 2)),
        "english" ->(1, Vector(1)),
        "frequencies" ->(1, Vector(1)),
        "generate" ->(1, Vector(1)),
        "given" ->(1, Vector(1)),
        "i.e." ->(1, Vector(1)),
        "in" ->(2, Vector(1, 2)),
        "label" ->(1, Vector(2)),
        "labeled" ->(1, Vector(1)),
        "list" ->(1, Vector(1)),
        "numbers" ->(1, Vector(2)),
        "occurrence" ->(1, Vector(2)),
        "occurrences" ->(1, Vector(1)),
        "of" ->(1, Vector(1)),
        "program" ->(1, Vector(1)),
        "sentence" ->(1, Vector(2)),
        "text" ->(1, Vector(1)),
        "that" ->(1, Vector(1)),
        "the" ->(1, Vector(2)),
        "which" ->(1, Vector(2)),
        "will" ->(1, Vector(1)),
        "with" ->(2, Vector(1, 2)),
        "word" ->(3, Vector(1, 1, 2)),
        "write" ->(1, List(1)),
        "written" ->(1, Vector(1))
      )

      val input = "Given an arbitrary text document written in English, write a program that will generate a \n\nconcordance, i.e. an alphabetical list of all word occurrences, labeled with word frequencies. \n\nBonus: label each word with the sentence numbers in which each occurrence appeared."

      Concordance.get(input.toStream)(Processor) must equalTo(expectedResult)

    }
  }

}
