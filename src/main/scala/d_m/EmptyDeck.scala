package d_m

case class EmptyDeck() extends Deck {
  def popTopCard(): (Option[Card], Deck) = (None, EmptyDeck())
  def shuffle(): Deck = EmptyDeck()
}