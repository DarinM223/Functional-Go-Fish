package d_m

object main {

  val NUM_DEAL_CARDS = 5

  def dealToPlayer(player: Player, deck: Deck, numTimes: Int): (Player, Deck) =
    if (numTimes == 0)
      (player, deck)
    else {
      val (card, newDeck) = deck.popTopCard()
      dealToPlayer(player.addCard(card.getOrElse(Card(1, Hearts()))), newDeck, numTimes - 1)
    }

  def main (args: Array[String]): Unit = {
    val _players = List(PersonPlayer("darin", List(), 0), ComputerPlayer("Bot1", List(), 0, Map[Int, List[Player]]()),
                       ComputerPlayer("Bot2", List(), 0, Map[Int, List[Player]]()))
    val (players, deck) = _players.foldLeft((Map[String, Player](), CardUtils.standardDeck().asInstanceOf[Deck]))((tuple, player) => tuple match {
      case (players, deck) => {
        val (newPlayer, newDeck) = dealToPlayer(player, deck, NUM_DEAL_CARDS)
        (players + (newPlayer.name -> newPlayer), newDeck)
      }
    })

    println(players)

    players.foreach({
      case (name, p: PersonPlayer) => p.turn(NoState(), "Bot2", Game(deck, Map(), players, p.name, "Bot1", false))
      case _ => ;
    })
  }
}
