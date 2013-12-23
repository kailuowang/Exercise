package com.kailuowang.concordance

object ConcordanceFromFile {
  def main(args: Array[String]) {

    implicit val processor = com.kailuowang.naturalLanguage.naive.Processor
    val file = args(0)
    val occurrences = Concordance.get(scala.io.Source.fromFile(file).iter)

    occurrences.foreach {
      case (word, (num, sentenceLabels)) =>
        println(s"${word} \t {${num}:${sentenceLabels.mkString(",")}}")
    }
  }
}
