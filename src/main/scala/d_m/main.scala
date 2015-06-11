package d_m

object main {
  class WonListenerImpl extends WonListener {
    def won(player: Player) = println(player.name + " has won!!")
  }

  def printPlayerState: PartialFunction[PlayerState, Unit] = {
    case PlayerState(name, numCards, piles) => println("Player " + name + " has " + numCards + " cards in the hand and " +
      piles + " piles")
  }

  def main (args: Array[String]): Unit = {
    val wonListener = new WonListenerImpl
    val game = new Game(1, List(new Player("darin"), new Player("user2")))
    game.addWonListener(wonListener)

    game.showCurrentGameState("user2").getOrElse(List()).foreach(playerState => printPlayerState(playerState))
  }
}
