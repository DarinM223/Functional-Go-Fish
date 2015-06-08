package d_m

case class NonemptyDeck(deck: List[Card]) extends Deck {
  def popTopCard(): (Option[Card], Deck) = deck match {
    case top::List() => (Some(top), EmptyDeck())
    case top::rest => (Some(top), NonemptyDeck(rest))
  }
  def shuffle(): Deck = NonemptyDeck(util.Random.shuffle(deck.toSeq).toList)
}