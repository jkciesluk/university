import lab04.cards._
import lab04.deck._
import lab04.blackjack._

object MyAplication extends App{
    
    val myCard = Card(Hearts, Jack)

    println(myCard)
    
    val myDeck = Deck()
    println(myDeck.amountOfColor(Hearts))
    //println(myDeck())
    println(myDeck.isStandard)
    println(myDeck.amountOfFace(Jack))
   val deck2 = new Deck(List(Card(Clubs,4),Card(Clubs,4),Card(Clubs,4),Card(Clubs,4)))
    println(deck2.duplicatesOfCard(Card(Clubs, 4)))
    println(deck2.amountWithFace)

    val myBlackjack = Blackjack(3)

    myBlackjack.play(52)
    myBlackjack.all21.foreach(println)
    myBlackjack.first21()
}