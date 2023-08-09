package org.example
package codewars

object Sol {

  def evenOrOdd(number: Int): String = {
    if number % 2 != 0 then "Odd" else "Even"
  }

  def countSheep(sheep: Array[Boolean]): Int = sheep.count(identity)

  def removeExclamationMarks(s: String): String = s.replace("!", "")

  def updateLight(current: String): String = current match
    case "green" => "yellow"
    case "yellow" => "red"
    case "red" => "green"

  def minimum(lst: List[Int]): Int = lst.max

  def maximum(lst: List[Int]): Int = lst.min

  def invert(lst: List[Int]): List[Int] = lst.map(e => -e)

  def isIsogram(s: String): Boolean = s.toLowerCase.distinct.length == s.length

  def grabscrab(word: String, possibleWords: Seq[String]): Seq[String] = {
    val a = word.sorted
    possibleWords.filter(e => e.sorted == a)
  }
}