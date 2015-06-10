package d_m

import org.scalatest._
import org.scalatest.events.TestFailed

class PersonPlayerSpec extends FlatSpec with Matchers {
  "PersonPlayer's hasCard" should "properly check if a player has a card" in {
    val player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(2, Spades()), Card(3, Diamonds())), 0)

    player.hasCard(1) should be (true)
    player.hasCard(2) should be (true)
    player.hasCard(3) should be (true)
    player.hasCard(4) should be (false)
  }

  "PersonPlayer's addCard" should "properly add card to hand" in {
    val player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(2, Spades())), 0)
    val newPlayer = player.addCard(Card(3, Hearts()))
    newPlayer.name should be (player.name)
    newPlayer.piles should be (player.piles)
    newPlayer.cards.length should be (3)
    newPlayer.cards.contains(Card(1, Hearts())) should be (true)
    newPlayer.cards.contains(Card(2, Spades())) should be (true)
    newPlayer.cards.contains(Card(3, Hearts())) should be (true)
  }

  it should "remove books when you add a card" in {
    val player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(1, Spades()), Card(1, Diamonds())), 0)
    val newPlayer = player.addCard(Card(1, Clubs()))
    newPlayer.cards.isEmpty should be (true)
  }

  "PersonPlayer's removeCard" should "properly remove card from hand" in {
    var player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(2, Spades()), Card(3, Diamonds())), 0)

    player = player.removeCard(1) match {
      case p: PersonPlayer => p
    }
    player.cards should be (Vector(Card(2, Spades()), Card(3, Diamonds())))
    player = player.removeCard(2) match {
      case p: PersonPlayer => p
    }
    player.cards should be (Vector(Card(3, Diamonds())))
    player = player.removeCard(3) match {
      case p: PersonPlayer => p
    }
    player.cards.isEmpty should be (true)
  }

  "PersonPlayer's removeBooks" should "not remove anything less than a 4 of a kind" in {
    val player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(1, Spades()), Card(1, Diamonds()),
      Card(2, Hearts()), Card(2, Spades()), Card(3, Clubs())), 0)
    val newPlayer = player.removeBooks()

    newPlayer.cards.length should be (player.cards.length)
    newPlayer.cards should be (player.cards.reverse)
    newPlayer.piles should be (0)
  }

  it should "remove a book and increase piles by 1" in {
    val player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(1, Spades()),
      Card(2, Hearts()), Card(1, Diamonds()), Card(2, Diamonds()), Card(1, Clubs())), 0)
    val newPlayer = player.removeBooks()

    newPlayer.cards.length should be (2)
    newPlayer.cards.contains(Card(2, Hearts())) should be (true)
    newPlayer.cards.contains(Card(2, Diamonds())) should be (true)
    newPlayer.piles should be (1)
  }

  it should "remove only 4 if there are more than 4 cards of the same number" in {
    val player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(1, Spades()),
      Card(2, Hearts()), Card(1, Diamonds()), Card(2, Diamonds()), Card(1, Clubs()),
      Card(1, Spades()), Card(1, Hearts())), 0)
    val newPlayer = player.removeBooks()

    newPlayer.cards.length should be (4)
    newPlayer.cards.contains(Card(2, Hearts())) should be (true)
    newPlayer.cards.contains(Card(2, Diamonds())) should be (true)
    newPlayer.cards.contains(Card(1, Spades())) should be (true)
    newPlayer.cards.contains(Card(1, Hearts())) should be (true)
    newPlayer.piles should be (1)
  }

  it should "remove everything if there are 8 of the same card" in {
    val player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(1, Spades()),
      Card(1, Diamonds()), Card(1, Clubs()), Card(1, Hearts()), Card(1, Spades()),
      Card(1, Diamonds()), Card(1, Clubs())), 0)
    val newPlayer = player.removeBooks()

    newPlayer.cards.isEmpty should be (true)
    newPlayer.piles should be (2)
  }

  "PersonPlayer's query" should "get another player's card if they have a card of same rank" in {
    val player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(2, Spades()), Card(3, Diamonds())), 0)
    val player2 = PersonPlayer("Sample2", List(Card(4, Clubs()), Card(7, Diamonds()), Card(9, Spades())), 0)
    val deck = CardUtils.standardDeck()

    val (newPlayer1, newPlayer2, newDeck) = player.query(7, player2, deck)
    newPlayer1.cards should be (List(Card(7, Diamonds()), Card(1, Hearts()), Card(2, Spades()), Card(3, Diamonds())).reverse)
    newPlayer2.cards should be (List(Card(4, Clubs()), Card(9, Spades())))

    newDeck match {
      case d: NonemptyDeck => d.deck should be (deck.deck)
    }
  }

  it should "draw from deck if other person doesn't have the card" in {
    val player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(2, Spades()), Card(3, Diamonds())), 0)
    val player2 = PersonPlayer("Sample", List(Card(2, Diamonds())), 0)
    val deck = CardUtils.standardDeck()

    val (newPlayer1, newPlayer2, newDeck) = player.query(7, player2, deck)
    newPlayer1.cards.length should be (4)
    newPlayer1.cards.contains(Card(1, Hearts())) should be (true)
    newPlayer1.cards.contains(Card(2, Spades())) should be (true)
    newPlayer1.cards.contains(Card(3, Diamonds())) should be (true)
    newPlayer2.cards should be (List(Card(2, Diamonds())))

    newDeck match {
      case d: NonemptyDeck => d.deck.length should be (51)
    }
  }

  it should "create a new deck if person doesn't have card and deck is empty" in {
    val player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(2, Spades()), Card(3, Diamonds())), 0)
    val player2 = PersonPlayer("Sample", List(Card(2, Diamonds())), 0)
    val deck = EmptyDeck()

    val (newPlayer1, newPlayer2, newDeck) = player.query(7, player2, deck)
    newPlayer1.cards.length should be (4)
    newPlayer1.cards.contains(Card(1, Hearts())) should be (true)
    newPlayer1.cards.contains(Card(2, Spades())) should be (true)
    newPlayer1.cards.contains(Card(3, Diamonds())) should be (true)
    newPlayer2.cards should be (List(Card(2, Diamonds())))

    newDeck.isInstanceOf[NonemptyDeck] should be (true)

    newDeck match {
      case d: NonemptyDeck => d.deck.length should be (51)
    }
  }
}
