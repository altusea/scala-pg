package org.example
package codewars

object Text extends App {

  def order(str: String): String = {
    if (str == "") then "" else {
      val a = str.split(" ")
      val order = a.map(e => e.filter(Character.isDigit).toInt)
      order.zip(a).sortBy(_._1).map(_._2).mkString(" ")
    }
  }
}