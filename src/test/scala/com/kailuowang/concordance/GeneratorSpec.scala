package com.kailuowang.concordance

import org.specs2.mutable.Specification

class GeneratorSpec extends Specification {
   "toString" should {
     "return some string" in {
        Generator.toString must contain("concordance")
     }
   }

}
