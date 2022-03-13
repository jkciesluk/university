package lab06.orders

import lab06.pizza._
import lab06.pizzeria._

object PhoneValidator {
    def isValid(phone: String): Boolean = phone.matches("""(^\+\d{11})|(^\d{9})$""") 
}


class Order(
        name: String,
        address: String,
        phone: String, //mandatory validated phone-number (hint: regex)
        pizzas: List[Pizza], 
        drinks: List[Drink],
        discount: Option[Discount] = None, //optional value
 
        specialInfo: Option[String] = None, //optional text, like: “Ring doesnt work,please knock”
    ) {
        require(PhoneValidator.isValid(phone), "Invaid phone number")
        /*
        val adress: String = adress
        val phone: String = phone
        val pizzas: List[Pizza] = pizzas
        val drinks: List[Drinks] = drinks
        val discount: Option[Discount] = discount
        val specialInfo = specialInfo
        */
        override def toString() = {
            val pizzasText: String = (for (pizza <- pizzas) yield "- "+ pizza + "\n").mkString
            val drinksText: String = (for (drink <- drinks) yield "- "+ drink + "\n").mkString
            val discountText = discount.map(_.toString).getOrElse("No Discount")
            f"Order for $name \n Address: $address, phone number: $phone \nPizzas: \n$pizzasText Drinks: \n$drinksText Discount: $discountText"
        }
    
        def extraMeatPrice: Option[Double] = {
            val price = pizzas.map(_.extraMeat.map(_.price).getOrElse(0.0)).sum
            price match {
                case a if a > 0=> Some(a)
                case _ => None
            }
        }    
        def pizzasPrice: Option[Double] = {
            val price = pizzas.map(_.price).sum
            price match {
                case a if a > 0=> Some(a)
                case _ => None   
            }
        }
        def drinksPrice: Option[Double] = {
            val price = drinks.map(_.price).sum
            price match {
                case a if a > 0=> Some(a)
                case _ => None
            }
        }
        def priceByType(kind: String): Option[Double] = pizzas.filter(pizza => pizza.kind.toString == kind).map(_.price).sum match {
                                                case a if a > 0 => Some(a)
                                                case _ => None       
        }

        val price: Double =  {
            val pPrice = pizzasPrice.getOrElse(0.0)
            val dPrice = drinksPrice.getOrElse(0.0)
            discount match {
                case None => pPrice + dPrice
                case Some(x) => x match {
                    case StudentDiscount => pPrice * 0.95 + dPrice
                    case SeniorDiscount => (pPrice + dPrice) * 0.93
                    case _ => pPrice + dPrice
                }
            }
        }
            
    }