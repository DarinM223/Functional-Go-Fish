package d_m

import org.scalatest._

class CardUtilsSpec extends FlatSpec with Matchers {
  "CardUtilsSpec's removeBooks" should "not remove anything less than a 4 of a kind" in {
    val player = PersonPlayer("Sample", Vector(Card(1, Hearts()), Card(1, Spades()), Card(1, Diamonds()),
      Card(2, Hearts()), Card(2, Spades()), Card(3, Clubs())), 0)
    val newPlayer = CardUtils.removeBooks(player)

    newPlayer.cards.length should be (player.cards.length)
    newPlayer.cards should be (player.cards.reverse)
    newPlayer.piles should be (0)
  }

  it should "remove a book and increase piles by 1" in {
    val player = PersonPlayer("Sample", Vector(Card(1, Hearts()), Card(1, Spades()),
      Card(2, Hearts()), Card(1, Diamonds()), Card(2, Diamonds()), Card(1, Clubs())), 0)
    val newPlayer = CardUtils.removeBooks(player)

    newPlayer.cards.length should be (2)
    newPlayer.cards.contains(Card(2, Hearts())) should be (true)
    newPlayer.cards.contains(Card(2, Diamonds())) should be (true)
    newPlayer.piles should be (1)
  }

  it should "remove only 4 if there are more than 4 cards of the same number" in {
    val player = PersonPlayer("Sample", Vector(Card(1, Hearts()), Card(1, Spades()),
      Card(2, Hearts()), Card(1, Diamonds()), Card(2, Diamonds()), Card(1, Clubs()),
      Card(1, Spades()), Card(1, Hearts())), 0)
    val newPlayer = CardUtils.removeBooks(player)

    newPlayer.cards.length should be (4)
    newPlayer.cards.contains(Card(2, Hearts())) should be (true)
    newPlayer.cards.contains(Card(2, Diamonds())) should be (true)
    newPlayer.cards.contains(Card(1, Spades())) should be (true)
    newPlayer.cards.contains(Card(1, Hearts())) should be (true)
    newPlayer.piles should be (1)
  }

  it should "remove everything if there are 8 of the same card" in {
    val player = PersonPlayer("Sample", Vector(Card(1, Hearts()), Card(1, Spades()),
      Card(1, Diamonds()), Card(1, Clubs()), Card(1, Hearts()), Card(1, Spades()),
      Card(1, Diamonds()), Card(1, Clubs())), 0)
    val newPlayer = CardUtils.removeBooks(player)

    newPlayer.cards.isEmpty should be (true)
    newPlayer.piles should be (2)
  }

  "CardUtilSpec's standardDeck" should "return a standard 52 card deck" in {
    val deck = CardUtils.standardDeck()
    deck.deck.length should be (52)
    deck.deck.filter(_.suite == Hearts()).map(_.number) should be (1 to 13)
    deck.deck.filter(_.suite == Spades()).map(_.number) should be (1 to 13)
    deck.deck.filter(_.suite == Diamonds()).map(_.number) should be (1 to 13)
    deck.deck.filter(_.suite == Clubs()).map(_.number) should be (1 to 13)
  }
}
