package d_m

import scala.util.control.Breaks._
import scala.collection.mutable

class BotPlayer(override val name: String, val parentGame: Game) extends Player(name) with TurnListener {
  parentGame.addTurnListener(this)

  private val playerGuesses = mutable.Map[Int, List[Player]]()

  private def savePrevState(prevState: RoundState) = prevState match {
    case RoundState(player, askedPlayer, query, successful) =>
      // add guess card if the card number hasn't already been discarded and guessed player hasn't already been added
      if (!DiscardPile.hasCard(query) && !playerGuesses.getOrElse(query, List()).contains(player)) {
        playerGuesses(query) = player::playerGuesses.getOrElse(query, List())
      }
  }

  def turn(prevState: RoundState, player: Player) = {
    savePrevState(prevState)
    if (player.name == this.name) {
      val currState = parentGame.showCurrentGameState(this.name).getOrElse(List())

      // build map ordered by # of same cards with rank in hand
      val map = this.cards.foldLeft(Map[Int, Int]())( (map, card) => map.get(card.number) match {
        case Some(num) => map.updated(card.number, num + 1)
        case None => map + (card.number -> 1)
      })

      // sort map based on # of cards of same number in hand
      val askCards = map.toList.sortBy(_._2).reverse
    }
  }
}
