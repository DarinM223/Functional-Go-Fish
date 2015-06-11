package d_m

import org.scalatest._

class PlayerSpec extends FlatSpec with Matchers {
  "Player's hasCard" should "properly check if a player has a card" in {
    val player = new Player("Sample")
    player.addCard(Card(1, Hearts()))
    player.addCard(Card(2, Spades()))
    player.addCard(Card(3, Diamonds()))

    player.hasCard(1) should be (true)
    player.hasCard(2) should be (true)
    player.hasCard(3) should be (true)
    player.hasCard(4) should be (false)
  }

  "Player's addCard" should "properly add card to hand" in {
    val player = new Player("Sample")
    player.addCard(Card(1, Hearts()))
    player.addCard(Card(2, Spades()))
    player.addCard(Card(3, Hearts()))
    player.piles should be (0)
    player.cards.length should be (3)
    player.cards.contains(Card(1, Hearts())) should be (true)
    player.cards.contains(Card(2, Spades())) should be (true)
    player.cards.contains(Card(3, Hearts())) should be (true)
  }

  it should "remove books when you add a card" in {
    val player = new Player("Sample")
    player.addCard(Card(1, Hearts()))
    player.addCard(Card(1, Spades()))
    player.addCard(Card(1, Diamonds()))
    player.addCard(Card(1, Clubs()))
    player.cards.isEmpty should be (true)
    player.piles should be (1)
  }

  "Player's removeCard" should "properly remove card from hand" in {
    val player = new Player("Sample")
    player.addCard(Card(1, Hearts()))
    player.addCard(Card(2, Spades()))
    player.addCard(Card(3, Diamonds()))

    player.removeCard(1)
    player.cards should be (List(Card(2, Spades()), Card(3, Diamonds())))
    player.removeCard(2)
    player.cards should be (List(Card(3, Diamonds())))
    player.removeCard(3)
    player.cards.isEmpty should be (true)
  }

  "PersonPlayer's query" should "get another player's card if they have a card of same rank" in {
    val player = new Player("Sample")
    player.addCard(Card(1, Hearts()))
    player.addCard(Card(2, Spades()))
    player.addCard(Card(3, Diamonds()))

    val player2 = new Player("Sample2")
    player2.addCard(Card(4, Clubs()))
    player2.addCard(Card(7, Diamonds()))
    player2.addCard(Card(9, Spades()))

    val deck = CardUtils.standardDeck()

    val (newDeck, isEmpty) = player.query(7, player2, deck)

    player.cards should be (List(Card(7, Diamonds()), Card(2, Spades()), Card(1, Hearts()), Card(3, Diamonds())).reverse)
    player2.cards should be (List(Card(4, Clubs()), Card(9, Spades())))

    newDeck match {
      case d: NonemptyDeck => d.deck should be (deck.deck)
    }

    isEmpty should be (false)
  }

  it should "draw from deck if other person doesn't have the card" in {
    val player = new Player("Sample")
    player.addCard(Card(1, Hearts()))
    player.addCard(Card(2, Spades()))
    player.addCard(Card(3, Diamonds()))

    val player2 = new Player("Sample")
    player2.addCard(Card(2, Diamonds()))

    val deck = CardUtils.standardDeck()

    val (newDeck, isEmpty) = player.query(7, player2, deck)

    player.cards.length should be (4)
    player.cards.contains(Card(1, Hearts())) should be (true)
    player.cards.contains(Card(2, Spades())) should be (true)
    player.cards.contains(Card(3, Diamonds())) should be (true)
    player2.cards should be (List(Card(2, Diamonds())))

    newDeck match {
      case d: NonemptyDeck => d.deck.length should be (51)
    }

    isEmpty should be (false)
  }

  it should "return false for second parameter if person doesn't have card and deck is empty" in {
    val player = new Player("Sample")
    player.addCard(Card(1, Hearts()))
    player.addCard(Card(2, Spades()))
    player.addCard(Card(3, Diamonds()))

    val player2 = new Player("Sample")
    player2.addCard(Card(2, Diamonds()))

    val deck = EmptyDeck()

    val (newDeck, isEmpty) = player.query(7, player2, deck)
    player.cards.length should be (3)
    player.cards.contains(Card(1, Hearts())) should be (true)
    player.cards.contains(Card(2, Spades())) should be (true)
    player.cards.contains(Card(3, Diamonds())) should be (true)
    player2.cards should be (List(Card(2, Diamonds())))

    newDeck.isInstanceOf[EmptyDeck] should be (true)

    isEmpty should be (true)
  }
}
