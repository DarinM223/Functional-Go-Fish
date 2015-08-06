package d_m

object main {

  val NUM_DEAL_CARDS = 5

  def dealToPlayer(
      player: Player,
      deck: Deck,
      discardPile: Map[Int, Boolean],
      numTimes: Int): (Player, Deck, Map[Int, Boolean]) = {

    if (numTimes == 0)
      (player, deck, discardPile)
    else {
      val (card, newDeck) = deck.popTopCard()
      val (newPlayer, newDiscardPile) = player.addCard(card.getOrElse(Card(1, Suite.Hearts)), discardPile)
      dealToPlayer(newPlayer, newDeck, newDiscardPile, numTimes - 1)
    }
  }


  def main (args: Array[String]): Unit = {
    val _players = List(
      PersonPlayer("darin", List(), 0),
      ComputerPlayer("Bot1", List(), 0),
      ComputerPlayer("Bot2", List(), 0)
    )

    val (players, deck, discardPile) = _players.foldLeft((
        Map[String, Player](),
        CardUtils.standardDeck().asInstanceOf[Deck],
        Map[Int, Boolean]())) { (tuple, player) =>

      tuple match {
        case (players, deck, discardPile) =>
          val (newPlayer, newDeck, newDiscardPile) = dealToPlayer(player, deck, discardPile, NUM_DEAL_CARDS)
          (players + (newPlayer.name -> newPlayer), newDeck, newDiscardPile)
      }
    }

    println(discardPile)

    val circularList = Iterator.continually(players.map(_._2.name)).flatten

    var game = Game(
      deck,
      discardPile,
      Map[Int, List[Log]](),
      players,
      circularList.next(),
      circularList.next(),
      false
    )

    var cardNumber: Int = 0

    while (!game.won) {
      val currPlayer: String = game.currentPlayer
      val nextPlayer: String = circularList.next()

      game.players.get(currPlayer) match {
        case Some(p: PersonPlayer) =>
          val (newCardNumber, newGame) = p.turn(nextPlayer, game)
          cardNumber = newCardNumber
          game = newGame
        case Some(p: ComputerPlayer) =>
          println("Bot " + p.name + "'s turn")
          val (newCardNumber, newGame) = p.turn(nextPlayer, game)
          cardNumber = newCardNumber
          game = newGame
        case None => println("Error with player name!")
      }
    }
  }
}
