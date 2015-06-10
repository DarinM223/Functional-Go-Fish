package d_m

trait Player {
  def query(rank: Int, player: Player, deck: Deck): (Player, Player, Deck) =
    if (player.hasCard(rank))
      (addCard(player.cards(player.cards.indexWhere(_.number == rank))), player.removeCard(rank), deck)
    else {
      val (optionCard, _deck) = deck.popTopCard()

      // If deck is empty, create a new deck and get the top card
      val (card, newDeck) = optionCard match {
        case Some(card) => (Some(card), _deck)
        case None => {
          val newDeck = CardUtils.standardDeck()
          newDeck.popTopCard()
        }
      }

      (addCard(card.getOrElse(Card(1, Hearts()))), player, newDeck)
    }

  val name: String
  val piles: Int
  val cards: List[Card]

  def removeCard(rank: Int): Player
  def addCard(card: Card): Player
  def removeBooks(): Player

  def hasCard(rank: Int): Boolean = cards.exists(_.number == rank)

  /*
   * Use these functions when implementing removeCard and addCard
   */

  /**
   * Removes books (4 cards with the same number) from the players hand
   * the order of cards in the resulting players hand is not guaranteed to be the same as before
   * @param newPlayer function that returns a new player with cards and piles updated
   * @return a player with the books removed
   */
  def removeBooks(newPlayer: (List[Card], Int) => Player): Player = {
    val map = cards.foldLeft(Map[Int, Int]())((map, card) => map.get(card.number) match {
      case Some(value) => map.updated(card.number, value + 1)
      case None => map + (card.number -> 1)
    })

    val (newCards, _) = cards.foldLeft((List[Card](), map))((tuple, card) => tuple match {
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

    newPlayer(newCards, newCount)
  }

  def removeCard(rank: Int, newPlayer: (List[Card]) => Player): Player =
    if (hasCard(rank))
      newPlayer(cards.patch(cards.indexWhere(_.number == rank), Nil, 1))
    else
      this

  def addCard(card: Card, newPlayer: (List[Card], Int) => Player): Player =
    newPlayer(card::cards, piles).removeBooks(newPlayer)
}