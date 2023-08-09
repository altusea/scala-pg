package org.example
package codewars

object Kata {

  def pyramid(n: Int): List[List[Int]] = {
    Range.inclusive(1, n).map(i => Range.inclusive(1, i).map(_ => 1).toList).toList
  }

  def groupByCommas(n: Int): String = {
    if (n < 1000) {
      n.toString
    } else {
      groupByCommas(n / 1000) + "," + (n % 1000).toString
    }
  }

  def validBraces(s: String): Boolean = ???
}