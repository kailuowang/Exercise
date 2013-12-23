Concordance
---------------

Given an arbitrary text document written in English, write a program that will generate a
concordance, i.e. an alphabetical list of all word occurrences, labeled with word frequencies.
Bonus: label each word with the sentence numbers in which each occurrence appeared.

Requires
---------------
* [sbt 0.13.1](https://scala-sbt.org]




Usage
---------------

As library:

    import com.kailuowang.concordance.Concordance

    Concordance.get(input)  //input can be either an iterator of Char or a scalaZ EphemeralStream

In command Line, you can read from a file:

    sbt

    run "PATH_TO_YOUR_FILE"



Performance
---------------

Time: O(n)

n: the number of characters.

On my machine, it takes 5 secs to parse a 6.5MB file, 95 secs to parse a 116.8MB file.

On average parses 1.22MB of text / sec

Memory: O(n)

n: the number of word appearance.


To Run Tests
---------------
    sbt
    test
