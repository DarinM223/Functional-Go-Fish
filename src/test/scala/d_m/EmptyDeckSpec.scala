package d_m

import org.scalatest._

class EmptyDeckSpec extends FlatSpec with Matchers {
  "An empty deck" should "return no card and an empty deck when popTopCard is called" in {
    val emptyDeck = EmptyDeck()
    emptyDeck.popTopCard() should be (None, EmptyDeck())
  }

  it should "return empty deck when shuffle is called" in {
    val emptyDeck = EmptyDeck()
    emptyDeck.shuffle() should be (EmptyDeck())
  }
}
