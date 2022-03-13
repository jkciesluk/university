package lab06.pizza

abstract class PizzaType(val price: Double)
case object Margarita extends PizzaType(5.0) {
    override def toString() = "Margarita"
}
case object Pepperoni extends PizzaType(6.5) {
    override def toString() = "Pepperoni"
}
case object Funghi extends PizzaType(7.0) {
    override def toString() = "Funghi"
}

abstract class PizzaSize(val priceMultiplier: Double)
case object Small extends PizzaSize(0.9) {
    override def toString() = "Small"
}
case object Medium extends PizzaSize(1) {
    override def toString() = "Medium"
}
case object Big extends PizzaSize(1.5) {
    override def toString() = "Big"
}


abstract class PizzaCrust()
case object Thick extends PizzaCrust() {
    override def toString() = "thick"
}
case object Thin extends PizzaCrust() {
    override def toString() = "thin"
}

abstract class Meat(val price: Double)
case object Salami extends Meat(1.0) {
    override def toString() = "salami"
}

abstract class Topping(val price: Double)
case object Ketchup extends Topping(0.5) {
    override def toString() = "ketchup"
}
case object Garlic extends Topping(0.5) {
    override def toString() = "garlic"
}


case class  Pizza(
     kind: PizzaType,
     size: PizzaSize, 
     crust: PizzaCrust,
     extraMeat: Option[Meat] = None,   //optional meat
     extraTopping: Option[Topping] = None //optional topping
) {
   
   override def toString() = {
    val extraMeatText = extraMeat.map(", with "+ _.toString).getOrElse("")
    val extraToppingText = extraTopping.map(", with "+ _.toString).getOrElse("")
    f"$size $kind on $crust crust$extraMeatText$extraToppingText"  
   }

   val  price: Double = (kind.price + extraMeat.map(_.price).getOrElse(0.0) + extraTopping.map(_.price).getOrElse(0.0)) * size.priceMultiplier
}

