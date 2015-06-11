package d_m

import scala.collection.mutable

trait WonListener {
  def won(player: Player)
}

trait TurnListener {
  def turn(prevState: RoundState, player: Player)
}

case class RoundState(player: Player, askedPlayer: Player, query: Int, successful: Boolean)
case class PlayerState(name: String, numCards: Int, piles: Int)

class Game(val id: Int, val _players: List[Player]) {
  private var deck: Deck = CardUtils.standardDeck()
  private val players = mutable.Map[String, Player]()

  _players.foreach((player) => players.put(player.name, player)) // load players from list into map

  private val nextPlayerCircular = Iterator.continually(players).flatten

  // Deal 5 cards to each player
  deck = deck.shuffle()
  val NUM_DEAL_CARDS = 5
  players.foreach({
    case (_, player) => {
      1 to NUM_DEAL_CARDS foreach { _ =>
        val tuple = deck.popTopCard()

        deck = tuple._2
        player.addCard(tuple._1.getOrElse(Card(1, Hearts())))
      }
    }
  })

  private var currentPlayer = nextPlayerCircular.next()._1
  private var _won = false
  private var wonListeners = List[WonListener]()
  private var turnListeners = List[TurnListener]()

  def won = _won

  def addWonListener(listener: WonListener) = wonListeners = listener::wonListeners
  def removeWonListener(listener: WonListener) = wonListeners = wonListeners.filter(_ != listener)

  def addTurnListener(listener: TurnListener) = turnListeners = listener::turnListeners
  def removeTurnListener(listener: TurnListener) = turnListeners = turnListeners.filter(_ != listener)

  private def broadcastWon(player: Player) = wonListeners.foreach({
    case listener => listener.won(player)
  })

  private def broadcastTurn(prevState: RoundState, player: Player) = turnListeners.foreach({
    case listener => listener.turn(prevState, player)
  })

  def showCurrentGameState(playerName: String): Option[List[PlayerState]] = players.get(playerName) match {
    case Some(player) => Some(players.filter({
      case (_, player) => player.name != playerName
    }).map({
      case (_, player) => PlayerState(player.name, player.cards.count((c) => true), player.piles)
    }).toList)
    case None => None
  }

  def query(askerName: String, targetName: String, rank: Int): Boolean =
    if (currentPlayer == askerName && players.get(targetName).isInstanceOf[Some[String]] && !_won) {
      val (newDeck, hadCard, ranOut) = players.getOrElse(askerName, new Player("Test"))
                                     .query(rank, players.getOrElse(targetName, new Player("Test")), deck)
      deck = newDeck
      if (ranOut) { // if the deck ran out, find the winning player and broadcast the winner
        val (_, wonPlayer) = players.maxBy({
          case (_, player) => player.piles
        })
        broadcastWon(wonPlayer)
        _won = true
      } else { // otherwise move to next player
        currentPlayer = nextPlayerCircular.next()._1
        broadcastTurn(RoundState(players.getOrElse(askerName, new Player("Test")),
                                 players.getOrElse(targetName, new Player("Test")),
                                 rank, hadCard), players.getOrElse(currentPlayer, new Player("Test")))
      }
      true
    } else {
      false
    }
}
