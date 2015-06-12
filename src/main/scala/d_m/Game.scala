package d_m

trait GameState
case class PrevState(card: Int, player: Player, game: Game) extends GameState
case class NoState() extends GameState

case class Log(player1: Player, player2: Player, success: Boolean)

case class Game(deck: Deck, discardPile: Map[Int, Boolean], logs: Map[Int, List[Log]], players: Map[String, Player],
                currentPlayer: String, nextPlayer: String, won: Boolean) {

  def query(askerName: String, targetName: String, nextPlayer: String, rank: Int): (Boolean, Game) =
    if (currentPlayer == askerName && players.get(targetName).isInstanceOf[Some[String]] && !won) {
      players.getOrElse(askerName, PersonPlayer("Test", List(), 0))
             .query(rank, players.getOrElse(targetName, PersonPlayer("Test", List(), 0)), deck, discardPile) match {
        case Player.QueryResult(newPlayer1, newPlayer2, newDeck, newDiscardPile, successful, ranOut) => {
          val newPlayers = players.updated(newPlayer1.name, newPlayer1).updated(newPlayer2.name, newPlayer2)
          if (ranOut) { // if the deck ran out, find the winning player and broadcast the winner
            (successful, copy(players = newPlayers, deck = newDeck, logs = logs.updated(rank, Log(newPlayer1, newPlayer2, successful)::logs.getOrElse(rank, List())), discardPile = newDiscardPile, won = true))
          } else { // otherwise move to next player
            (successful, copy(players = newPlayers, deck = newDeck, logs = logs.updated(rank, Log(newPlayer1, newPlayer2, successful)::logs.getOrElse(rank, List())),discardPile = newDiscardPile, currentPlayer = this.nextPlayer, nextPlayer = nextPlayer))
          }
        }
        case _ => (false, this)
      }
    } else {
      println("Error: Player supposed to be " + currentPlayer)
      (false, this)
    }
}
