package d_m

case class PersonPlayer(override val name: String, override val cards: List[Card], override val piles: Int)
  extends Player(name, cards, piles) with Turnable {

  override def copy(name: String, cards: List[Card], piles: Int) = PersonPlayer(name, cards, piles)

  override def turn(prevState: GameState, nextPlayer: String, game: Game): (Player, Int, Game) = {
    println("It is " + this.name + "'s turn")
    game match {
      case Game(deck, discardPile, players, currentPlayer, nextPlayer, won) => {
        players.foreach({
          case (_, player) => println("Player " + player.name + " has " + player.cards.count(_ => true) + " and " + player.piles + " piles")
        })

        println("The discard pile contains card with numbers: " + discardPile.map({ case (num, _) => num}))
      }
    }

    println("Enter the card you want to query")
    val cardNumber = scala.io.StdIn.readInt()

    val playerName = scala.io.StdIn.readLine("Enter the player name you want to query")

    (this, cardNumber, game.query(this.name, playerName, nextPlayer, cardNumber))
  }
}