

  import scala.io.StdIn._

  object PrimeNumber extends App {

    def gcd(a: Int, b: Int): Int = b match {
      case 0 => a
      case b if (b > a) => gcd(b, a)
      case _ => gcd(b, a % b)
    }

    def isPrime(n: Int, i: Int): Boolean = n match {
      case n if n == i => true

      case n if gcd(n, i) > 1 => false

      case x => isPrime(n, i + 1)

    }


    print("Enter a Number : ")
    var n = readInt()

    println(isPrime(n, 2))



}
