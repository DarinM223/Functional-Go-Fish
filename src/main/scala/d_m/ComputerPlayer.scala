package d_m

case class ComputerPlayer(override val name: String, override val cards: List[Card], override val piles: Int,
                          override val playerGuesses: Map[Int, List[Player]]) extends BotPlayer(name, cards, piles, playerGuesses) {
  override def copyBotPlayer(name: String, cards: List[Card], piles: Int, playerGuesses: Map[Int, List[Player]]) = ComputerPlayer(name, cards, piles, playerGuesses)
}
