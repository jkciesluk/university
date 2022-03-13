package lab04.cards

class CardValue(val value: Int){
    require(value > 0 && value < 14)
    override def toString: String = value match {
        case 11 => "Jack"
        case 12 => "Queen"
        case 13 => "King"
        case 1  => "Ace"
        case other => f"$other"
    }
}

object CardValue {
    def apply(value: Int) = new CardValue(value)
}


case object Queen extends CardValue(12)
case object King extends CardValue(13)
case object Jack extends CardValue(11)
case object Ace extends CardValue(1)


abstract class CardColor(val color: String){
    //require(Set("Hearts", "Clubs", "Diamonds", "Spades").contains(color))
    override def toString: String = color
}


case object Hearts extends CardColor("Hearts")
case object Diamonds extends CardColor("Diamonds")
case object Clubs extends CardColor("Clubs")
case object Spades extends CardColor("Spades")



case class Card(color: CardColor, rank: CardValue) {
    def isFace(): Boolean = rank.value > 10
    def isNumerical(): Boolean = rank.value > 1 && rank.value < 11
    def isColor(c: CardColor): Boolean = color == c
    def isValue(f: CardValue): Boolean = rank == f
    override def toString: String = f"$rank of $color"
    
}

object Card {
    def apply(color: CardColor, rank:CardValue): Card = new Card(color, rank)
    def apply(color: CardColor, rank:Int): Card = new Card(color, new CardValue(rank))
    val ranks = List(CardValue(2), CardValue(3), CardValue(4), CardValue(5), CardValue(6), CardValue(7), CardValue(8), CardValue(9), CardValue(10), Jack, Queen, King, Ace)
    val colors = List(Hearts, Diamonds, Spades, Clubs)
}
