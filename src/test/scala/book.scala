object book extends App{
  def totalPrice(copies:Double)=24.95*copies*0.4*copies+3.0*50.0+0.75*10.0;

  println(totalPrice(60))

}
