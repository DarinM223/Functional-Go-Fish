package d_m

object CardUtils {
  def removeBooks(player: Player): Player = {
    val map = player.cards.foldLeft(Map[Int, Int]())((map, card) => map.get(card.number) match {
      case Some(value) => map.updated(card.number, value + 1)
      case None => map + (card.number -> 1)
    })

    val newCards = player.cards.filter((card) => map.get(card.number) match {
      case Some(num) => num < 4
      case None => true
    })

    player match {
      case PersonPlayer(name, cards, piles) => PersonPlayer(name, newCards, piles)
      case BotPlayer(name, cards, piles) => BotPlayer(name, newCards, piles)
    }
  }
}
