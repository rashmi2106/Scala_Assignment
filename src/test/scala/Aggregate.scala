object Aggregate extends App {
  val lst1 = List(12,33,11, 3)
  val lst2 = List("a", "b", "c","d")
  val lst = lst1 zip lst2
  val list = lst.map(x => List(x._1, x._2))
  println(list)
}
