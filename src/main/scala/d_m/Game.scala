package d_m

trait GameState
case class PrevState(card: Int, player: Player, game: Game) extends GameState
case class NoState() extends GameState

case class Game(deck: Deck, discardPile: Map[Int, Boolean], players: Map[String, Player],
                currentPlayer: String, nextPlayer: String, won: Boolean) {

  def query(askerName: String, targetName: String, nextPlayer: String, rank: Int): Game =
    if (currentPlayer == askerName && players.get(targetName).isInstanceOf[Some[String]] && !won) {
      val (newPlayer1, newPlayer2, newDeck, ranOut) = players.getOrElse(askerName, PersonPlayer("Test", List(), 0))
        .query(rank, players.getOrElse(targetName, PersonPlayer("Test", List(), 0)), deck)

      val newPlayers = players.updated(newPlayer1.name, newPlayer1).updated(newPlayer2.name, newPlayer2)
      if (ranOut) { // if the deck ran out, find the winning player and broadcast the winner
        copy(players = newPlayers, deck = newDeck, won = true)
      } else { // otherwise move to next player
        copy(players = newPlayers, deck = newDeck, currentPlayer = this.nextPlayer, nextPlayer = nextPlayer)
      }
    } else
      this
}
