package d_m

case class PersonPlayer(name: String, cards: List[Card], piles: Int) extends Player {
  def removeCard(rank: Int): Player = removeCard(rank, (cards) => PersonPlayer(name, cards, piles))
  def addCard(card: Card): Player = addCard(card, (cards, piles) => PersonPlayer(name, cards, piles))
  def removeBooks(): Player = removeBooks((cards: List[Card], piles: Int) => PersonPlayer(name, cards, piles))
}
