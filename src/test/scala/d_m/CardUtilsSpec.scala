package d_m

import org.scalatest._

class CardUtilsSpec extends FlatSpec with Matchers {
  "CardUtilSpec's standardDeck" should "return a standard 52 card deck" in {
    val deck = CardUtils.standardDeck()
    deck.deck.length should be (52)
    deck.deck.filter(_.suite == Suite.Hearts).map(_.number) should be (1 to 13)
    deck.deck.filter(_.suite == Suite.Spades).map(_.number) should be (1 to 13)
    deck.deck.filter(_.suite == Suite.Diamonds).map(_.number) should be (1 to 13)
    deck.deck.filter(_.suite == Suite.Clubs).map(_.number) should be (1 to 13)
  }
}
