package d_m

abstract class Player(val name: String, val cards: List[Card], val piles: Int) {
  def copy(name: String = name, cards: List[Card] = cards, piles: Int = piles): Player

  /**
   * Asks another player if they have a certain card
   * @param rank
   * @param player
   * @param deck
   * @return the new player1, the new player2, the new deck, and a boolean that is true when the deck runs out (so that the game will end)
   */
  def query(rank: Int, player: Player, deck: Deck): (Player, Player, Deck, Boolean) =
    if (player.hasCard(rank)) {
      (addCard(player.cards(player.cards.indexWhere(_.number == rank))), player.removeCard(rank), deck, false)
    } else {
      val (optionCard, newDeck) = deck.popTopCard()

      optionCard match {
        case Some(card) => {
          (addCard(card), player, newDeck, false)
        }
        case None => (this, player, newDeck, true)
      }
    }

  /**
   * Removes books (4 cards with the same number) from the players hand
   * the order of cards in the resulting players hand is not guaranteed to be the same as before
   */
  def removeBooks(): Player = {
    val map = cards.foldLeft(Map[Int, Int]())((map, card) => map.get(card.number) match {
      case Some(value) => map.updated(card.number, value + 1)
      case None => map + (card.number -> 1)
    })

    val (newCards, _) = cards.foldLeft((List[Card](), map))((tuple, card) => tuple match {
      case (cards, map) => map.get(card.number) match {
        case Some(num) => num match {
          case num if num < 4 => (card::cards, map)
          case num if num % 4 == 0 => {
            if (!DiscardPile.hasCard(num)) // add card number to discard pile if not already there
              DiscardPile.addCard(num)
            (cards, map)
          }
          case _ => (card::cards, map.updated(card.number, num - 1))
        }
        case None => (card::cards, map)
      }
    })

    val addAmount: Int = map.foldLeft(0)((count, keyValue) => count + Math.floorDiv(keyValue._2, 4))

    copy(cards = newCards, piles = piles + addAmount)
  }

  def hasCard(rank: Int) = cards.exists(_.number == rank)
  def addCard(card: Card): Player = copy(cards = card::cards)
  def removeCard(rank: Int): Player =
    if (hasCard(rank))
      copy(cards = cards.patch(cards.indexWhere(_.number == rank), Nil, 1))
    else
      this
}