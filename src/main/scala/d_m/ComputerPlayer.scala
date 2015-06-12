package d_m

case class ComputerPlayer(override val name: String, override val cards: List[Card], override val piles: Int) extends BotPlayer(name, cards, piles) {
  override def copy(name: String, cards: List[Card], piles: Int) = ComputerPlayer(name, cards, piles)
}
