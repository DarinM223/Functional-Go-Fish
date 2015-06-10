package d_m

import org.scalatest._

class NonemptyDeckSpec extends FlatSpec with Matchers {
  "A nonempty deck" should "return the top card of the deck and the rest of the deck in popTopCard" in {
    val deck = NonemptyDeck(List(Card(1, Hearts()), Card(2, Spades()), Card(3, Diamonds())))
    deck.popTopCard() should be (Some(Card(1, Hearts())), NonemptyDeck(List(Card(2, Spades()), Card(3, Diamonds()))))
  }

  it should "return an empty deck if there is only one card in the deck" in {
    val deck = NonemptyDeck(List(Card(1, Hearts())))
    deck.popTopCard() should be (Some(Card(1, Hearts())), EmptyDeck())
  }

  it should "shuffle the deck when shuffle is called" in {
    val deck = NonemptyDeck(List(Card(1, Hearts()), Card(2, Spades()), Card(3, Diamonds())))
    val newDeck = deck.shuffle() match {
      case newDeck: NonemptyDeck => newDeck
    }

    newDeck.deck.length should be (deck.deck.length)
  }
}
