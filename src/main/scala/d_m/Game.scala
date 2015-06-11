package d_m

import scala.collection.mutable

trait WonListener {
  val id: Int
  def won(player: Player)
}

class Game(val id: Int) {
  private var deck: Deck = EmptyDeck()
  private val players = mutable.Map[String, Player]()
  private val nextPlayerCircular = Iterator.continually(players).flatten

  private var currentPlayer = nextPlayerCircular.next()._1
  private val wonListeners = mutable.Map[Int, WonListener]()

  def addWonListener(listener: WonListener) = wonListeners += (listener.hashCode() -> listener)
  def removeWonListener(listener: WonListener) = wonListeners.remove(listener.hashCode())

  private def broadcastWon(player: Player) = wonListeners.foreach({
    case (_, listener) => listener.won(player)
  })

  def query(askerName: String, targetName: String, rank: Int): Boolean =
    if (currentPlayer == askerName && players.get(targetName).isInstanceOf[Some[String]]) {
      val (newDeck, ranOut) = players.getOrElse(askerName, new Player("Test"))
                                     .query(rank, players.getOrElse(targetName, new Player("Test")), deck)
      deck = newDeck
      if (ranOut) { // if the deck ran out, find the winning player and broadcast the winner
        val (_, wonPlayer) = players.maxBy({
          case (_, player) => player.piles
        })
        broadcastWon(wonPlayer)
      } else { // otherwise move to next player
        currentPlayer = nextPlayerCircular.next()._1
      }
      true
    } else {
      false
    }
}
