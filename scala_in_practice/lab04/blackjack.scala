package lab04.blackjack

import lab04.deck._
import lab04.cards._
import scala.util.Random

class Blackjack(val deck: Deck) {
    /* Points calculation:
    1. Numerical cards as their numerical value = 2 - 10.  
    2. Face cards (Jack, Queen, King) = 10 
    3. Ace = 1 or 11 (player could choose)
    */
    def play(n: Int): Unit = {
        for (card <- deck().take(n)) {
            print(f"$card: ")
            println(card.rank match {
                case Jack|Queen|King => 10
                case Ace => "1 or 11"
                case number => number
            })
        }
    }


    //helper functions for all21 and first21

    private def newRes(card: Card, res: Int): Int = {
        card.rank match {
            case Jack|Queen|King => res + 10
            case Ace => if(res > 10) res + 1 else res + 11
            case number => res + number.value
        }
    }
        
    private def find21(xs: List[Card], acc:List[Card], res: Int): List[Card] = {
        if(res == 21)  acc
        else if(xs.isEmpty || res > 21) List.empty[Card]
        else find21(xs.tail, acc :+ xs.head, newRes(xs.head, res))
    }

    lazy val all21: List[List[Card]] = {
        deck().tails.foldLeft(List.empty[List[Card]])((acc, tail) => {
            find21(tail, List.empty[Card], 0) match {
                case Nil => acc
                case blackjack => acc :+  blackjack 
            }
        })
        //other solution using map and filter
        //deck().tails.map((tail) => find21(tail, List.empty[Card], 0)).filter(!_.isEmpty).toList
    }
    
    def first21(): Unit = {
        val first = deck().tails.map((tail) => find21(tail, List.empty[Card], 0)).filter(!_.isEmpty).next()
        println("First 21: ")
        first.foreach(println)
    }
        
}



object Blackjack {
    def apply(numOfDecks: Int) = {
        val cards = List.fill(numOfDecks)(Deck().cards).flatten
        val shuffled = Random.shuffle(cards)
        new Blackjack(new Deck(shuffled))
    }
}



