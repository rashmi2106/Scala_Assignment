object movieTicket extends App  {

  import scala.io.StdIn._



    def attendee(price: Double) = 120 + (15 - price) / 5 * 20

    def revenue(price: Double) = price * attendee(price)

    def cost(price: Double) = 500 + 3 * attendee(price)

    def profit(price: Double) = revenue(price) - cost(price)

    printf("Enter the Ticket Price :")
    var tprice = readDouble()

    printf("Profit for Given Ticket Price : %.2f \n ", profit(tprice))



}
