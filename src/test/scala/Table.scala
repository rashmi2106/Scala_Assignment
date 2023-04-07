/**
print the table of each element in the List
 */

object Table extends App {
  val list = List(1,2,3,4)
  for(i<-list)
  {
    for(j<-1 to 10)
    {
      print(i*j+" ")

    }
    println("")

  }

}
