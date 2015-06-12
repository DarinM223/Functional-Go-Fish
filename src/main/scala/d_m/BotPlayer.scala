package d_m

import scala.util.control.Breaks._

abstract class BotPlayer(override val name: String, override val cards: List[Card], override val piles: Int,
                         val playerGuesses: Map[Int, List[Player]])
  extends Player(name, cards, piles) with Turnable {

  def copyBotPlayer(name: String = name, cards: List[Card] = cards, piles: Int = piles,
           playerGuesses: Map[Int, List[Player]] = playerGuesses): Player

  override def copy(name: String, cards: List[Card], piles: Int): Player = copyBotPlayer(name = name, cards = cards, piles = piles,
    playerGuesses = playerGuesses)

  private def savePrevGame(prevGame: Game, card: Int, player: Player): Player = prevGame match {
    case Game(deck, discardPile, players, currentPlayer, _, won)  =>
      // add guess card if the card number hasn't already been discarded and guessed player hasn't already been added
      if (!discardPile.getOrElse(card, false) && !playerGuesses.getOrElse(card, List()).contains(player)) {
        copyBotPlayer(playerGuesses = playerGuesses.updated(card, player::playerGuesses.getOrElse(card, List())))
      } else
        this
  }

  def turn(prevState: GameState, nextPlayer: String, game: Game): (Player, Int, Game) = {
    val newPlayer = (prevState match {
      case PrevState(card, player, game) => savePrevGame(game, card, player)
      case NoState() => this
    }).asInstanceOf[BotPlayer]

    if (newPlayer.name == this.name) {
      // build map ordered by # of same cards with rank in hand
      val map = this.cards.foldLeft(Map[Int, Int]())( (map, card) => map.get(card.number) match {
        case Some(num) => map.updated(card.number, num + 1)
        case None => map + (card.number -> 1)
      })

      // sort map based on # of cards of same number in hand
      val askCards = map.toList.sortBy(_._2).reverse

      var queryPlayer: String = nextPlayer
      var queryCardNumber: Int = -1

      breakable {
        askCards.foreach({
          case (cardNumber, numInHand) => playerGuesses.get(cardNumber) match {
            case Some(player::rest) => {
              queryPlayer = player.name
              queryCardNumber = queryCardNumber
              break()
            }
          }
        })
      }

      val (_, newGame) = game.query(this.name, queryPlayer, nextPlayer, queryCardNumber)

      (newPlayer, queryCardNumber, newGame)
    } else
      (newPlayer, 0, game)
  }
}



