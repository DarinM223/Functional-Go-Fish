package d_m

trait Turnable {
  def turn(nextPlayer: String, game: Game): (Int, Game)
}
