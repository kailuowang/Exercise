Concordance
---------------


To Run Tests
---------------
    sbt
    test

Requires
---------------
* [sbt 0.13.1](https://scala-sbt.org]


Problem
---------------

Given an arbitrary text document written in English, write a program that will generate a
concordance, i.e. an alphabetical list of all word occurrences, labeled with word frequencies.
Bonus: label each word with the sentence numbers in which each occurrence appeared.

Usage
---------------
    import com.kailuowang.concordance.Concordance

    Concordance.get
