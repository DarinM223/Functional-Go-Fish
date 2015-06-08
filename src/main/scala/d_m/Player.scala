package d_m

trait Player {
  def query(rank: Int, player: Player, deck: Deck): (Player, Player, Deck) = if (player.hasCard(rank))
    (addCard(player.cards(player.cards.indexWhere(_.number == rank))), player.removeCard(rank), deck)
  else {
    val (optionCard, newDeck) = deck.popTopCard()
    val card = optionCard.getOrElse(Card(1, Hearts())) // TODO: fix sample code

    (addCard(card), player, newDeck)
  }

  val name: String
  val piles: Int
  val cards: Vector[Card]

  def hasCard(rank: Int): Boolean
  def removeCard(rank: Int): Player
  def addCard(card: Card): Player
}