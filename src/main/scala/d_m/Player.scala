package d_m

object Player {
  case class QueryResult(player1: Player, player2: Player, deck: Deck, discardPile: Map[Int, Boolean], successful: Boolean, ranOut: Boolean)
}

abstract class Player(val name: String, val cards: List[Card], val piles: Int) {
  def copy(name: String = name, cards: List[Card] = cards, piles: Int = piles): Player

  /**
   * Asks another player if they have a certain card
   * @param rank
   * @param player
   * @param deck
   * @param discardPile
   * @return the new player1, the new player2, the new deck, the new discard pile,
   * a boolean that is true if the query is successful and a boolean that is true when the deck runs out (so that the game will end)
   */
  def query(rank: Int, player: Player, deck: Deck, discardPile: Map[Int, Boolean]): Player.QueryResult =
    if (player.hasCard(rank)) {
      val (newThis, newDiscardPile) = addCard(player.cards.find(_.number == rank).getOrElse(Card(1, Hearts())), discardPile)
      Player.QueryResult(newThis, player.removeCard(rank), deck, newDiscardPile, true, false)
    } else {
      val (optionCard, newDeck) = deck.popTopCard()

      optionCard match {
        case Some(card) => {
          val (newPlayer, newDiscardPile) = addCard(card, discardPile)
          Player.QueryResult(newPlayer, player, newDeck, newDiscardPile, false, false)
        }
        case None => Player.QueryResult(this, player, newDeck, discardPile, false, true)
      }
    }

  /**
   * Removes books (4 cards with the same number) from the players hand
   * the order of cards in the resulting players hand is not guaranteed to be the same as before
   */
  def removeBooks(discardPile: Map[Int, Boolean]): (Player, Map[Int, Boolean]) = {
    val map = cards.foldLeft(Map[Int, Int]())((map, card) => map.get(card.number) match {
      case Some(value) => map.updated(card.number, value + 1)
      case None => map + (card.number -> 1)
    })

    val (newCards, _, newDiscardPile) = cards.foldLeft((List[Card](), map, discardPile))((tuple, card) => tuple match {
      case (cards, map, discardPile) => map.get(card.number) match {
        case Some(num) => num match {
          case num if num < 4 => (card::cards, map, discardPile)
          case num if num % 4 == 0 => {
            if (discardPile.getOrElse(card.number, false)) // add to discard pile if not already
              (cards, map, discardPile)
            else
              (cards, map, discardPile + (card.number -> true))
          }
          case _ => (card::cards, map.updated(card.number, num - 1), discardPile)
        }
        case None => (card::cards, map, discardPile)
      }
    })

    val addAmount: Int = map.foldLeft(0)((count, keyValue) => count + Math.floorDiv(keyValue._2, 4))

    (copy(cards = newCards, piles = piles + addAmount), newDiscardPile)
  }

  def hasCard(rank: Int) = cards.exists(_.number == rank)
  def addCard(card: Card, discardPile: Map[Int, Boolean]): (Player, Map[Int, Boolean]) = copy(cards = card::cards).removeBooks(discardPile)

  // TODO: edit removeCard to remove all of same rank and return list of removed cards
  def removeCard(rank: Int): Player = if (hasCard(rank)) copy(cards = cards.patch(cards.indexWhere(_.number == rank), Nil, 1)) else this
}