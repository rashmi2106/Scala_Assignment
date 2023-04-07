import scala.io.StdIn._
object encription {





  def main(args: Array[String]) {

    val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    //READ WORD
    print("Enter the Word >")
    var word = readLine()
    //Shifted key ?
    print("Enter Shifted key >")
    var shift = readInt()
    //ASK E OR D
    print("Enter E-Encrypt or D-Decrypy >")
    val flag: Char = readChar().toUpper

    val E = (c: Char, shift: Int, a: String) => a((a.indexOf(c.toUpper) + shift) % 26)

    val D = (c: Char, shift: Int, a: String) => a((a.indexOf(c.toUpper) - shift) % 26)


    val cipher = (algo: (Char, Int, String) => Char, word: String, a: String, shift: Int) => word.map(algo(_, shift, a));

    //CALL

    val c = if (flag == 'E') cipher(E, word, alphabet, shift) else cipher(D, word, alphabet, shift)
    println(c);


  }

}
