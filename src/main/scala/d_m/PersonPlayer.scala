package d_m

case class PersonPlayer(name: String, cards: Vector[Card], piles: Int) extends Player {
  def hasCard(rank: Int): Boolean = cards.exists(_.number == rank)

  def removeCard(rank: Int): Player = if (hasCard(rank))
    PersonPlayer(name, cards.patch(cards.indexWhere(_.number == rank), Nil, 1), piles)
  else
    this

  def addCard(card: Card): Player = CardUtils.removeBooks(PersonPlayer(name, cards :+ card, piles))
}
