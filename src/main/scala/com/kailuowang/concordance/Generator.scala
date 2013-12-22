package com.kailuowang.concordance
import DataTypes._
import com.kailuowang.naturalLanguage.Processor


class Generator(implicit lp: Processor) {
  def get(input: Stream[Char]): Map[String, Occurrences] = ???
}
