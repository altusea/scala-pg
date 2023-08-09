println("Hello, world!")

val x = 1
x + x

def f(a: Int, b: Int): Int = a + b

f(1, 2)

def toTupleList(a: List[String]): List[Tuple2[String, Int]] = {
  a.map(s => Tuple2.apply(s, s.length))
}

val a = List("a", "bbb", "ccccc")
toTupleList(a).foreach(println)
