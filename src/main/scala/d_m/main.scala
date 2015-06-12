package d_m

object main {

  val NUM_DEAL_CARDS = 5

  def dealToPlayer(player: Player, deck: Deck, discardPile: Map[Int, Boolean], numTimes: Int): (Player, Deck, Map[Int, Boolean]) =
    if (numTimes == 0)
      (player, deck, discardPile)
    else {
      val (card, newDeck) = deck.popTopCard()
      val (newPlayer, newDiscardPile) = player.addCard(card.getOrElse(Card(1, Hearts())), discardPile)
      dealToPlayer(newPlayer, newDeck, newDiscardPile, numTimes - 1)
    }

  def main (args: Array[String]): Unit = {
    val _players = List(PersonPlayer("darin", List(), 0), ComputerPlayer("Bot1", List(), 0, Map[Int, List[Player]]()),
                       ComputerPlayer("Bot2", List(), 0, Map[Int, List[Player]]()))
    val (players, deck, discardPile) = _players.foldLeft((Map[String, Player](), CardUtils.standardDeck().asInstanceOf[Deck], Map[Int, Boolean]()))((tuple, player) => tuple match {
      case (players, deck, discardPile) => {
        val (newPlayer, newDeck, newDiscardPile) = dealToPlayer(player, deck, discardPile, NUM_DEAL_CARDS)
        (players + (newPlayer.name -> newPlayer), newDeck, newDiscardPile)
      }
    })

    println(discardPile)

    players.foreach({
      case (name, p: PersonPlayer) => p.turn(NoState(), "Bot2", Game(deck, discardPile, players, p.name, "Bot1", false))
      case _ => ;
    })
  }
}
