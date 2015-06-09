package d_m

import org.scalatest._

class CardUtilsSpec extends FlatSpec with Matchers {
  "CardUtilsSpec's removeBooks" should "not remove anything less than a 4 of a kind" in {
    val player = PersonPlayer("Sample", Vector(Card(1, Hearts()), Card(1, Spades()), Card(1, Diamonds()),
      Card(2, Hearts()), Card(2, Spades()), Card(3, Clubs())), 0)
    val newPlayer = CardUtils.removeBooks(player)

    newPlayer.cards.length should be (player.cards.length)
    newPlayer.cards should be (player.cards)
  }

  // TODO: test that removeBooks increments piles

  "CardUtilSpec's standardDeck" should "return a standard 52 card deck" in {
    val deck = CardUtils.standardDeck()
    deck.deck.length should be (52)
    deck.deck.filter(_.suite == Hearts()).map(_.number) should be (1 to 13)
    deck.deck.filter(_.suite == Spades()).map(_.number) should be (1 to 13)
    deck.deck.filter(_.suite == Diamonds()).map(_.number) should be (1 to 13)
    deck.deck.filter(_.suite == Clubs()).map(_.number) should be (1 to 13)
  }
}
