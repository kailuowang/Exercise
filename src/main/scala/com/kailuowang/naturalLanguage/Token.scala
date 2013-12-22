package com.kailuowang.naturalLanguage

sealed trait Token {
  def value: String

}
case class Word(value: String) extends Token
case class Punctuation(value: String) extends Token
