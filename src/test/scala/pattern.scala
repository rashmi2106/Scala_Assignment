class Pattern
{
  def Opertion1(x:Int,y:String,z:Int):Int= y match{

    case "*" => x*z
    case "+" => x+z
    case "-" => x-z
    case "/" => x/z
    case "%" => x%z
  }



}
object mathematics  extends App{

  var s="2*3*4"
  var list1=List[Int]()
  var list2=List[String]()
  var sum=0
  for(i<-0 to s.length-1)
  {
    var y= (s.substring(i,i+1))
    if((i%2)==0)
    {
      var x = y.toInt
      list1=list1:+x
    }
    else list2=list2:+y

  }
  println(list1)
  println(list2)
  val obj1 = new  Pattern
  var i =0
  var j=0
  var l =list2.length
  var k=1
  while(l>0)
  {
    if(sum==0)
    {

      sum= obj1.Opertion1(list1(i),list2(j),list1(k))
      i=i+2
      j=j+1
    }
    else
    {
      sum=obj1.Opertion1(sum,list2(j),list1(i))
      i=i+1;j=j+1
    }
    l=l-1
  }
  println(sum)

}