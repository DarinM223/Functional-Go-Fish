package d_m

case class BotPlayer(name: String, cards: List[Card], piles: Int) extends Player {
  def removeCard(rank: Int): Player = removeCard(rank, (cards: List[Card]) => BotPlayer(name, cards, piles))
  def addCard(card: Card): Player = addCard(card, (cards: List[Card], piles) => BotPlayer(name, cards, piles))
  def removeBooks(): Player = removeBooks((cards: List[Card], piles: Int) => BotPlayer(name, cards, piles))
}
