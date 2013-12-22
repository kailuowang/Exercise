package com.kailuowang.concordance

import org.specs2.mutable.Specification
import com.kailuowang.naturalLanguage.NaiveProcessor

class ConcordanceSpec extends Specification {
   "Get" should {
     "work with the given example" in {
       val expectedResult = List(
         "a" -> (2, List(1,1)),
         "all" -> (1, List(1)),
         "alphabetical" -> (1, List(1)),
         "an" -> (2, List(1,1)),
         "appeared" -> (1, List(2)),
         "arbitrary" -> (1, List(1)),
         "bonus" -> (1, List(2)),
         "concordance" -> (1, List(1)),
         "document" -> (1, List(1)),
         "each" -> (2, List(2,2)),
         "english" -> (1, List(1)),
         "frequencies" -> (1, List(1)),
         "generate" -> (1, List(1)),
         "given" -> (1, List(1)),
         "i.e." -> (1, List(1)),
         "in" -> (2, List(1,2)),
         "label" -> (1, List(2)),
         "labeled" -> (1, List(1)),
         "list" -> (1, List(1)),
         "numbers" -> (1, List(2)),
         "occurrence" -> (1, List(2)),
         "occurrences" -> (1, List(1)),
         "of" -> (1, List(1)),
         "program" -> (1, List(1)),
         "sentence" -> (1, List(2)),
         "text" -> (1, List(1)),
         "that" -> (1, List(1)),
         "the" -> (1, List(2)),
         "which" -> (1, List(2)),
         "will" -> (1, List(1)),
         "with" -> (2, List(1,2)),
         "word" -> (3, List(1,1,2)),
         "write" -> (1, List(1)),
         "written" -> (1, List(1))
       )

       val input = "Given an arbitrary text document written in English, write a program that will generate a \n\nconcordance, i.e. an alphabetical list of all word occurrences, labeled with word frequencies. \n\nBonus: label each word with the sentence numbers in which each occurrence appeared."

       Concordance.get(input.toStream)(NaiveProcessor).toList must equalTo(expectedResult)

     }
   }

}
