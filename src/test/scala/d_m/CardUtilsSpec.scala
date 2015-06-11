package d_m

import org.scalatest._

class CardUtilsSpec extends FlatSpec with Matchers {
  "CardUtilSpec's standardDeck" should "return a standard 52 card deck" in {
    val deck = CardUtils.standardDeck()
    deck.deck.length should be (52)
    deck.deck.filter(_.suite == Hearts()).map(_.number) should be (1 to 13)
    deck.deck.filter(_.suite == Spades()).map(_.number) should be (1 to 13)
    deck.deck.filter(_.suite == Diamonds()).map(_.number) should be (1 to 13)
    deck.deck.filter(_.suite == Clubs()).map(_.number) should be (1 to 13)
  }
}
