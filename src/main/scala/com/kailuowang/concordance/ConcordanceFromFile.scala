package com.kailuowang.concordance

object ConcordanceFromFile {
  def main(args: Array[String]) {
    val rt = Runtime.getRuntime()

    val previous = rt.freeMemory()

    implicit val processor = com.kailuowang.naturalLanguage.naive.Processor
    val file = args(0)
    val fileStream = scala.io.Source.fromFile(file).iter.toStream

    val occurrences = Concordance.get(fileStream)

    occurrences.foreach {
      case (word, (num, sentenceLabels)) =>
        println(s"${word} \t {${num}:${sentenceLabels.mkString(",")}}")
    }


    val used = previous - rt.freeMemory()
    println( (used / 1024 / 1024) + "MB memory was consumed")


  }


}
