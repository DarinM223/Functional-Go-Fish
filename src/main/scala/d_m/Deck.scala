package d_m

trait Deck {
  val deck: List[Card]

  def popTopCard(): (Option[Card], Deck)
  def shuffle(): Deck
}
