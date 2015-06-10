package d_m

object main {
  def main (args: Array[String]): Unit = {
    val deck = CardUtils.standardDeck()

    println(deck.deck.length)

    var player = PersonPlayer("darin", List[Card](), 0)
    player = player.addCard(Card(1, Hearts())) match {
      case p: PersonPlayer => p
    }

    println(player.cards)

    player = player.addCard(Card(1, Hearts())) match {
      case p: PersonPlayer => p
    }

    println(player.cards)

    player = player.addCard(Card(1, Hearts())) match {
      case p: PersonPlayer => p
    }

    println(player.cards)

    player = player.addCard(Card(1, Hearts())) match {
      case p: PersonPlayer => p
    }

    println(player.cards)
  }
}
