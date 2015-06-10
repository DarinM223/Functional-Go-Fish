package d_m

case class EmptyDeck() extends Deck {
  val deck: List[Card] = List() // has empty deck

  def popTopCard(): (Option[Card], Deck) = (None, EmptyDeck())
  def shuffle(): Deck = EmptyDeck()
}