package d_m

object CardUtils {

  /**
   * Removes books (4 cards with the same number) from the players hand
   * the order of cards in the resulting players hand is not guaranteed to be the same as before
   * @param player the player to remove books from
   * @return a player with the books removed
   */
  def removeBooks(player: Player): Player = {
    val map = player.cards.foldLeft(Map[Int, Int]())((map, card) => map.get(card.number) match {
      case Some(value) => map.updated(card.number, value + 1)
      case None => map + (card.number -> 1)
    })

    val (newCards, _) = player.cards.foldLeft((List[Card](), map))((tuple, card) => tuple match {
      case (cards, map) => map.get(card.number) match {
        case Some(num) =>
          if (num < 4)
            (card::cards, map)
          else if (num % 4 == 0) // if divisible by 4 just remove all cards of that number
            (cards, map)
          else // if not divisible by 4 then subtract 1 from the total number of cards for every card
            (card::cards, map.updated(card.number, num - 1))
        case None => (card::cards, map)
      }
    })

    val newCount: Int = map.foldLeft(0)((count, keyValue) => count + Math.floorDiv(keyValue._2, 4))

    player match {
      case PersonPlayer(name, cards, piles) => PersonPlayer(name, newCards.toVector, newCount)
      case BotPlayer(name, cards, piles) => BotPlayer(name, newCards.toVector, newCount)
    }
  }

  /**
   * Returns a standard 52 card deck
   * @return a standard 52 card deck
   */
  def standardDeck(): NonemptyDeck = NonemptyDeck((for {
    num <- 1 to 13
    suite <- List(Hearts(), Diamonds(), Clubs(), Spades())
  } yield Card(num, suite)).toList)
}
