package d_m

object main {

  val NUM_DEAL_CARDS = 5

  def dealToPlayer(player: Player, deck: Deck, numTimes: Int): (Player, Deck) =
    if (numTimes == 0)
      (player, deck)
    else {
      val (card, newDeck) = deck.popTopCard()
      dealToPlayer(player.copy(cards = card.getOrElse(Card(1, Hearts()))::player.cards), newDeck, numTimes - 1)
    }

  def main (args: Array[String]): Unit = {
    val _players = List(PersonPlayer("darin", List(), 0), ComputerPlayer("Bot1", List(), 0, Map[Int, List[Player]]()),
                       ComputerPlayer("Bot2", List(), 0, Map[Int, List[Player]]()))
    val (players, deck) = _players.foldLeft((List[Player](), CardUtils.standardDeck().asInstanceOf[Deck]))((tuple, player) => tuple match {
      case (players, deck) => {
        val (newPlayer, newDeck) = dealToPlayer(player, deck, NUM_DEAL_CARDS)
        (newPlayer::players, newDeck)
      }
    })
    val circular = Iterator.continually(players).flatten

    var prevState: GameState = NoState()
    var game = Game(CardUtils.standardDeck(), Map(), Map(), circular.next().name, circular.next().name, false)
    var nextPlayer = circular.next().name

    println(game)

    circular.takeWhile({
      case p: PersonPlayer => {
        val (newPlayer, queryCard, newGame) = p.turn(prevState, nextPlayer, game)
        prevState = PrevState(queryCard, newPlayer, game)
        game = newGame.copy(players = newGame.players.updated(newPlayer.name, newPlayer))
        nextPlayer = circular.next().name
        !newGame.won
      }
      case a => println(a); true
    })
  }
}
