//queue using list

class Queue { //class to implement Queue using list
  def enqueue(element: Int, l: List[Int]): List[Int] = { //function to add element to the queue
    val myList = l :+ element
    myList
  }


  def dequeue(l: List[Int]) = { //function to delete element from the queue
    val myList = l.drop(1)
    myList
  }
}


object Queue1 extends App {

  val obj = new Queue //Instantiating class QueueUsingList
  val queuelist1 = obj.enqueue(4, List(1, 2, 3))
  println(s"The list after pushing element 5 is $queuelist1")
  val queuelist2 = obj.enqueue(6, List(1, 2, 3, 4, 6))
  println(s"The list after pushing element 6 is $queuelist2")
  val dequeuelist = obj.dequeue(queuelist2)
  println(s"The list after popping last element is $dequeuelist")
}
