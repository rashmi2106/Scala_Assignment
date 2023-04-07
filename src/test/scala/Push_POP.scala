/**
implement Stack using Lists.
 */
class Push_Pop
{
  def push(list:List[Int],value:Int)=
  {
    val list1=list
    val list2=List(value)
    val list3=list1:::list2

    list3
  }
  def pop(list:List[Int]):List[Int]=
  {
    val list1=list
    val list2=list1.init
    list2


  }

}
object Stack extends App{
  val lst=List[Int]()
  val obj1 = new Push_Pop()
  val newlist= obj1.push(lst, 1)
  println(newlist)
  val new1list= obj1.push(newlist, 2)
  println(new1list)
  val new2list= obj1.push(new1list, 3)
  println(new2list)

  val new3list=obj1.pop(new2list)
  println(new3list)
  val new4list=obj1.pop(new3list)
  println(new4list)
}