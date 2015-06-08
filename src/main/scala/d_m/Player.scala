package d_m

trait Player {
  def query(rank: Int, player: Player, deck: Deck): (Player, Player, Deck) = if (player.hasCard(rank))
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
  val cards: Vector[Card]

  def hasCard(rank: Int): Boolean
  def removeCard(rank: Int): Player
  def addCard(card: Card): Player
}