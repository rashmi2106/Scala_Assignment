import scala.io.StdIn._
object employeeSalary extends App{
  def wage(normalHour:Int)=normalHour*150
  def ot(otHour:Int)=otHour*75
  def tax(income:Int)=income*0.1
  def income(normalHour:Int,otHour:Int)=wage(normalHour)+ot(otHour)
  printf("Enter Normal Hour:")
  var normalHour=readInt()
  printf("Enter OT Hours:")
  var otHour=readInt()
  printf("Take home salary= %.2f\n",income(normalHour,otHour)-tax(income(normalHour,otHour)))

}
