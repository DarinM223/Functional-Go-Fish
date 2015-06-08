package d_m

trait Deck {
  def popTopCard(): (Option[Card], Deck)
  def shuffle(): Deck
}
