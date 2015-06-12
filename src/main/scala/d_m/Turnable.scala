package d_m

trait Turnable {
  def turn(prevState: GameState, nextPlayer: String, game: Game): (Player, Int, Game)
}
