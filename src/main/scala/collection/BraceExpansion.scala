package org.example
package collection

import scala.annotation.tailrec

// https://rosettacode.org/wiki/Brace_expansion#Scala
object BraceExpansion {

  case class State(isChild: Boolean, alts: Seq[String], rem: List[Char])

  def expand(s: String): Seq[String] = {

    @tailrec
    def parseGroup(s: State): State = s.rem match {
      case Nil => s.copy(alts = List("{" + s.alts.mkString(",")))
      case ('{' | ',') :: sp =>
        val newS = State(true, List(""), rem = sp)
        val elem = parseElem(newS)
        elem.rem match {
          case Nil => elem.copy(alts = elem.alts.map(a => "{" + s.alts.map(_ + ",").mkString("") + a))
          case elemrem => parseGroup(s.copy(alts = s.alts ++ elem.alts, rem = elem.rem))
        }
      case '}' :: sp =>
        if (s.alts.isEmpty) s.copy(alts = List("{}"), rem = sp)
        else if (s.alts.length == 1) s.copy(alts = List("{" + s.alts.head + "}"), rem = sp)
        else s.copy(rem = sp)
      case _ => throw new Exception("parseGroup should be called only with delimitors")
    }

    @tailrec
    def parseElem(s: State): State = s.rem match {
      case Nil => s
      case '{' :: sp =>
        val ys = parseGroup(State(true, List(), s.rem))
        val newAlts = for {x <- s.alts; y <- ys.alts} yield x + y
        parseElem(s.copy(alts = newAlts, rem = ys.rem))
      case (',' | '}') :: _ if s.isChild => s
      case '\\' :: c :: sp => parseElem(s.copy(alts = s.alts.map(_ + '\\' + c), rem = sp))
      case c :: sp => parseElem(s.copy(alts = s.alts.map(_ + c), rem = sp))
    }

    parseElem(State(false, List(""), s.toList)).alts
  }

  @main
  def main(): Unit = {
    println(expand("""~/{Downloads,Pictures}/*.{jpg,gif,png}""") mkString "\n")
    println(expand("It{{em,alic}iz,erat}e{d,}, please.") mkString "\n")
    println(expand("""{,{,gotta have{ ,\, again\, }}more }cowbell!""") mkString "\n")
    println(expand("""{}} some }{,{\\{ edge, edge} \,}{ cases, {here} \\\\\}""") mkString "\n")
  }
}
