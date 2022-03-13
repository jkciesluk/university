import lab06.pizza._
import lab06.pizzeria._
import lab06.orders._


object Application extends App{
    val pizza1 = Pizza(Margarita, Small, Thick, Some(Salami), None)
    val pizza2 = Pizza(Funghi, Medium, Thin, None, None)
    val pizza3 = Pizza(Pepperoni, Big, Thick, Some(Salami), Some(Ketchup))
    val pizza4 = Pizza(Pepperoni, Big, Thick, None, Some(Ketchup))
    val order1 = new Order("Jan Kochanowski", "ul. Bujwida 42", "+48123456789", 
    List(pizza1,pizza2, pizza3), List(Lemonade, Lemonade), Some(StudentDiscount))
    println(order1)
    println(order1.price)
    println(order1.pizzasPrice)
    println(order1.priceByType("Funghi"))
    
    //val wrongPhoneNumber = new Order("Adam Mickiewicz", "ul. Juliusz Słowackiego 44/19", "+891352431", List(pizza1, pizza2, pizza3, pizza4), List(), specialInfo = Some("Room in the basement"))
    
    val order2 = new Order("Adam Mickiewicz", "ul. Juliusz Słowackiego 44/19", "891352431", 
    List(pizza1, pizza2, pizza3, pizza4), List(), specialInfo = Some("Room in the basement"))
    println(order2)
    println(order2.price)
    println(order1.priceByType("Pepperoni"))
    
}