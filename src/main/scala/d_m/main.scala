package d_m

object main {
  def main (args: Array[String]): Unit = {
    val deck = CardUtils.standardDeck()

    println(deck.deck.length)

    val player = new Player("darin")
    player.addCard(Card(1, Hearts()))

    println(player.cards)

    player.addCard(Card(1, Hearts()))

    println(player.cards)

    player.addCard(Card(1, Hearts()))

    println(player.cards)

    player.addCard(Card(1, Hearts()))

    println(player.cards)
  }
}
