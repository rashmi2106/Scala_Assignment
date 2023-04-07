class Ratio(x:Int,y:Int) {
  var numer=x;
  var denom=y;

  def neg()= new Ratio(-numer,denom);
}

object First {
  def main(args: Array[String]) {
    val x:Ratio=new Ratio(3,4);
    //1. neg
    var r:Ratio=x.neg;
    println(r.numer+"/"+r.denom);


  }
}