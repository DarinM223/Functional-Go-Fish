package d_m

object Game {
  trait State
  case class PrevState(card: Int, player: Player, game: Game) extends State
  case class NoState() extends State
}

case class Log(player1: Player, player2: Player, success: Boolean)

case class Game(
    deck: Deck,
    discardPile: Map[Int, Boolean],
    logs: Map[Int, List[Log]],
    players: Map[String, Player],
    currentPlayer: String,
    nextPlayer: String,
    won: Boolean) {

  def query(
      askerName: String,
      targetName: String,
      nextPlayer: String,
      rank: Int): (Boolean, Game) =

    if (currentPlayer == askerName && players.get(targetName).isInstanceOf[Some[String]] && !won) {
      players.getOrElse(askerName, PersonPlayer("Test", List(), 0))
             .query(
                rank,
                players.getOrElse(targetName, PersonPlayer("Test", List(), 0)),
                deck,
                discardPile) match {

        case Player.QueryResult(
            newPlayer1,
            newPlayer2,
            newDeck,
            newDiscardPile,
            successful,
            ranOut) => {

          val newPlayers = players.updated(newPlayer1.name, newPlayer1)
                                  .updated(newPlayer2.name, newPlayer2)

          val newLogs = logs.updated(rank, Log(newPlayer1, newPlayer2, successful)::logs.getOrElse(rank, List()))

          if (ranOut) { // if the deck ran out, abort
            val returnedPlayer = copy(
              players = newPlayers,
              deck = newDeck,
              logs = newLogs,
              discardPile = newDiscardPile,
              won = true
            )
            (successful, returnedPlayer)
          } else { // otherwise move to next player
            val returnedPlayer = copy(
              players = newPlayers,
              deck = newDeck,
              logs = newLogs,
              discardPile = newDiscardPile,
              currentPlayer = this.nextPlayer,
              nextPlayer = nextPlayer
            )
            (successful, returnedPlayer)
          }
        }
        case _ => (false, this)
      }
    } else {
      println("Error: Player supposed to be " + currentPlayer)
      (false, this)
    }
}
