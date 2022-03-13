package lab06.pizzeria

abstract class Drink(val price: Double)
case object Lemonade extends Drink(1.0) {
    override def toString() = "Lemonade"
}

abstract class Discount()
case object StudentDiscount extends Discount() {
    override def toString() = "Student discount"
}
case object SeniorDiscount extends Discount() {
    override def toString() = "Senior discount"
}
