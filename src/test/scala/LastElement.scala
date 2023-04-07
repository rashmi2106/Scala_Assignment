/**
Find the last element of list with its index value(dont use inbuilt methods to extract last element directly)
 */

class Elements
{

  def lastElement(list:List[Int],index:Int):(Int,Int)= // using tail recursion
  {
    val element=list.head

    if((list.tail).isEmpty) (element,index) else lastElement(list.tail,index+1)
  }
}

object LastElement extends App {
  val list = List(1,2,3,4,9)
  val obj1 = new Elements
  val result=(obj1.lastElement(list,0))
  println("last elemnt="+result._1)
  println("index is ="+result._2)


}