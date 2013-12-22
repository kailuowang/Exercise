package com.kailuowang.naturalLanguage

sealed trait Token {
  def value: String
}

sealed trait Parser {
  def accept(char: Char) : Boolean
}

case class Word(value: String) extends Token

case class Punctuation(value: String) extends Token
