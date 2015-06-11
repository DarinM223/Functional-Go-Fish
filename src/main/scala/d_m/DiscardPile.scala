package d_m

import scala.collection.mutable

object DiscardPile {
  private val discardedCards = mutable.Map[Int, Boolean]()
  def addCard(cardNumber: Int) = discardedCards(cardNumber) = true
  def removeCard(cardNumber: Int) = discardedCards(cardNumber) = false
  def hasCard(cardNumber: Int) = discardedCards.get(cardNumber) match {
    case Some(b) => b
    case None => false
  }
}