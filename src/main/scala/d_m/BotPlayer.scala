package d_m

abstract class BotPlayer(override val name: String, override val cards: List[Card], override val piles: Int)
  extends Player(name, cards, piles) with Turnable {

  def turn(nextPlayer: String, game: Game): (Int, Game) = {
    // build map ordered by # of same cards with rank in hand
    val map = this.cards.foldLeft(Map[Int, Int]())((map, card) => map.get(card.number) match {
      case Some(num) => map.updated(card.number, num + 1)
      case None => map + (card.number -> 1)
    })

    // sort map based on # of cards of same number in hand
    val askCards = map.toList.sortBy(_._2).reverse

    val queryPlayer = nextPlayer
    val queryCardNumber: Int = askCards match {
      case (num, _) :: rest => num
    }

    println(this.name + " is querying " + queryPlayer + " for cards of rank " + queryCardNumber)

    // TODO: use game logs to analyze best card to ask for and best player to ask

    val (successful, newGame) = game.query(this.name, queryPlayer, nextPlayer, queryCardNumber)

    if (successful) {
      println(this.name + " took a card of rank " + queryCardNumber + " from " + queryPlayer)
    } else {
      println(this.name + " drew a card from the deck")
    }

    (queryCardNumber, newGame)
  }
}



