package lab04.deck

import lab04.cards._
import scala.util.Random

class Deck(val cards: List[Card]) {
    
    def apply(): List[Card] = cards
    def apply(index: Int): Card = cards(index)
    override def toString = f"$cards"

    def pull(): (Card, Deck) = (cards.head, new Deck(cards.tail))
    
    def push(c: Card): Deck = new Deck(c +: cards)
    
    def push(color: CardColor, value: CardValue): Deck = new Deck(Card(color, value) +: cards) 
    def push(color: CardColor, value: Int): Deck = new Deck(Card(color, value) +: cards) 
    

    private def sumIf(f: Card => Boolean)(startVal: Int): Int = {       // methods amountOfColor, amountWithNumerical, amountOfFace, amountWithFace look all the same
        cards.foldLeft(startVal)((acc, head) => {                       // so this method makes them shorter
            head match {
                case a if f(a) => acc + 1
                case _ => acc
            } 
            
        })
    }

    def duplicatesOfCard(card: Card): Int = {
        sumIf((a: Card) => a.rank.value == card.rank.value && a.color.color == card.color.color)(0)
    }

    val isStandard: Boolean = {
        if(cards.length != 52 ) false
        else {
            val duplicates = cards.map((card) => duplicatesOfCard(card))
            if(duplicates.filter((amount) => amount != 1).length > 0) false
            else true
        }
    }
    
    def amountOfColor(color: CardColor): Int = {
        sumIf(_.isColor(color))(0)
    }

    val amountWithNumerical: Int = {
        sumIf(_.isNumerical())(0)
    }

    def amountOfFace(face: CardValue) : Int = {
        sumIf(_.isValue(face))(0)
    }
    val amountWithFace: Int = {
        sumIf(_.isFace())(0)
    }
}

object Deck {
    def apply() = {
        val options = Card.ranks.flatMap((rank) => Card.colors.map((color) => Card(color, rank)))
        val shuffled = Random.shuffle(options)
        new Deck(shuffled)
    }
}

