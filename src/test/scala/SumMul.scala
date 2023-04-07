/**
 * find sum and multiplication of the list (dont use inbuilt methods)
 */

class SumMul {

  var lst3: List[Int] = List()
  var lst4: List[Int] = List()

  def sum(lst1: List[Int], lst2: List[Int]) = {
    var l1 = for {
      index <- 0 to lst1.length - 1
      lst3 = lst1(index) + lst2(index)
    } yield lst3
    println(l1)
  }

  def mul(lst1: List[Int], lst2: List[Int]) = {
    var l2 = for {
      index <- 0 to lst1.length - 1
      lst4 = lst1(index) * lst2(index)
    } yield lst4
    println(l2)
  }
}

object SumMulList extends App {
  val lst1 = List(1, 2, 3)
  val lst2 = List(2, 3, 4)
  val lst = new SumMul
  lst.sum(lst1, lst2)
  lst.mul(lst1, lst2)
}