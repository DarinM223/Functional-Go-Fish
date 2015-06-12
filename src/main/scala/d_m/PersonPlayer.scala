package d_m

import scala.util.control.Breaks._

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

    println("Your cards: " + this.cards.map({ case (card) => card.toString}))

    var cardNumber = 0
    var playerName = ""

    breakable {
        while (cardNumber > 13 || cardNumber < 1 || !game.players.contains(playerName)) {
          println("Enter the card you want to query")
          cardNumber = scala.io.StdIn.readInt()

          println("Enter the player name you want to query")
          playerName = scala.io.StdIn.readLine()

          if (cardNumber > 13 || cardNumber < 1 || !game.players.contains(playerName)) {
            println("Error")
          } else {
            break()
          }
      }
    }
    val (successful, newGame) = game.query(this.name, playerName, nextPlayer, cardNumber)
    if (successful) {
      println("Added card to hand")
    } else {
      println("Go fish!")
    }
    (this, cardNumber, newGame)
  }
}