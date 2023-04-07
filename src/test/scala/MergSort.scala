/**
apply mergesort on the Lists
 */

object MergeSort1 extends App
{
  def mergeSort(lst: List[Int]): List[Int] = {
    val num = lst.length / 2
    if (num == 0) lst
    else {
      def merge(lst: List[Int], lst1: List[Int]): List[Int] =
        (lst, lst1) match {
          case(Nil, lst1) => lst1
          case(lst, Nil) => lst
          case(x :: xs1, y :: ys1) =>
            if (x < y) x::merge(xs1, lst1)
            else y :: merge(lst, ys1)
        }
      val (left, right) = lst splitAt(num)
      merge(mergeSort(left), mergeSort(right))
    }
  }
  val list1=mergeSort(List(4,63,7,34,32,12,22,10))
  println(list1)
}
