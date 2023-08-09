package org.example
package codewars

object DeadFish {
  def parse(data: String): List[Int] = {
    data.foldLeft((0, List[Int]())) {
      case ((value, output), 'i') => (value + 1, output)
      case ((value, output), 'd') => (value - 1, output)
      case ((value, output), 's') => (value * value, output)
      case ((value, output), 'o') => (value, output :+ value)
      case ((value, output), _) => (value, output)
    }._2
  }
}