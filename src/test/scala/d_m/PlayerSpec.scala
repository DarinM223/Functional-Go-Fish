package d_m

import org.scalatest._

class PlayerSpec extends FlatSpec with Matchers {
  "PersonPlayer's hasCard" should "properly check if a player has a card" in {
    val player = PersonPlayer("Test", List(Card(1, Hearts()), Card(2, Spades()), Card(3, Diamonds())), 0)
    player.hasCard(1) should be (true)
    player.hasCard(2) should be (true)
    player.hasCard(3) should be (true)
    player.hasCard(4) should be (false)
  }

  "PersonPlayer's addCard" should "properly add card to hand" in {
    val player = PersonPlayer("Test", List(Card(1, Hearts()), Card(2, Spades())), 0)
    val discardPile = Map[Int, Boolean](4 -> true)
    val (newPlayer, newDiscardPile) = player.addCard(Card(3, Diamonds()), discardPile)

    newPlayer.piles should be (0)
    newPlayer.cards.length should be (3)
    newPlayer.cards.contains(Card(1, Hearts())) should be (true)
    newPlayer.cards.contains(Card(2, Spades())) should be (true)
    newPlayer.cards.contains(Card(3, Diamonds())) should be (true)

    newDiscardPile should be (discardPile)
  }

  it should "remove books when you add a card" in {
    val player = PersonPlayer("Test", List(Card(1, Hearts()), Card(1, Spades()), Card(1, Diamonds())), 0)
    val discardPile = Map[Int, Boolean](4 -> true)
    val (newPlayer, newDiscardPile) = player.addCard(Card(1, Clubs()), discardPile)

    newPlayer.cards.isEmpty should be (true)
    newPlayer.piles should be (1)
    newDiscardPile should be (Map[Int, Boolean](4 -> true, 1 -> true))
  }


  "PersonPlayer's removeCard" should "properly remove card from hand" in {
    val player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(2, Spades()), Card(3, Diamonds())), 0)

    var tuple = player.removeCard(1)
    var newPlayer = tuple._1
    var removedCards = tuple._2
    newPlayer.cards should be (List(Card(2, Spades()), Card(3, Diamonds())))
    removedCards.length should be (1)

    tuple = newPlayer.removeCard(2)
    newPlayer = tuple._1
    removedCards = tuple._2
    newPlayer.cards should be (List(Card(3, Diamonds())))
    removedCards.length should be (1)

    tuple = newPlayer.removeCard(3)
    newPlayer = tuple._1
    removedCards = tuple._2
    newPlayer.cards.isEmpty should be (true)
    removedCards.length should be (1)
  }

  it should "remove all cards of the rank" in {
    val player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(1, Spades()), Card(1, Diamonds())), 0)
    val (newPlayer, removedCards) = player.removeCard(1)
    newPlayer.cards.isEmpty should be (true)
    removedCards.length should be (3)
    removedCards.contains(Card(1, Hearts())) should be (true)
    removedCards.contains(Card(1, Spades())) should be (true)
    removedCards.contains(Card(1, Diamonds())) should be (true)
  }

  "PersonPlayer's query" should "get another player's card if they have a card of same rank" in {
    val player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(2, Spades()), Card(3, Diamonds())), 0)
    val player2 = PersonPlayer("Sample2", List(Card(4, Clubs()), Card(7, Diamonds()), Card(9, Spades())), 0)
    val deck = CardUtils.standardDeck()
    val discardPile = Map[Int, Boolean](10 -> true)

    player.query(7, player2, deck, discardPile) match {
      case Player.QueryResult(player1, player2, deck, discardPile, success, ranOut) => {
        player1.cards.contains(Card(7, Diamonds())) should be (true)
        player1.cards.contains(Card(2, Spades())) should be (true)
        player1.cards.contains(Card(1, Hearts())) should be (true)
        player1.cards.contains(Card(3, Diamonds())) should be (true)

        player2.cards.contains(Card(4, Clubs())) should be (true)
        player2.cards.contains(Card(9, Spades())) should be (true)

        discardPile should be (Map[Int, Boolean](10 -> true))

        deck match {
          case d: NonemptyDeck => d.deck should be (deck.deck)
        }

        success should be (true)
        ranOut should be (false)
      }
    }
  }

  it should "draw from deck if other person doesn't have the card" in {
    val player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(2, Spades()), Card(3, Diamonds())), 0)
    val player2 = PersonPlayer("Sample2", List(Card(2, Diamonds())), 0)
    val deck = CardUtils.standardDeck()
    val discardPile = Map[Int, Boolean](10 -> true)

    player.query(7, player2, deck, discardPile) match {
      case Player.QueryResult(player1, player2, deck, discardPile, success, ranOut) => {
        player1.cards.length should be (4)
        player1.cards.contains(Card(1, Hearts())) should be (true)
        player1.cards.contains(Card(2, Spades())) should be (true)
        player1.cards.contains(Card(3, Diamonds())) should be (true)
        player2.cards should be (List(Card(2, Diamonds())))

        deck match {
          case d: NonemptyDeck => d.deck.length should be(51)
        }

        success should be (false)
        ranOut should be (false)
      }
    }
  }

  it should "return false for second parameter if person doesn't have card and deck is empty" in {
    val player = PersonPlayer("Sample", List(Card(1, Hearts()), Card(2, Spades()), Card(3, Diamonds())), 0)
    val player2 = PersonPlayer("Sample2", List(Card(2, Diamonds())), 0)
    val deck = EmptyDeck()
    val discardPile = Map[Int, Boolean](10 -> true)

    player.query(7, player2, deck, discardPile) match {
      case Player.QueryResult(player1, player2, deck, discardPile, success, ranOut) => {
        player1.cards.length should be (3)
        player1.cards.contains(Card(1, Hearts())) should be (true)
        player1.cards.contains(Card(2, Spades())) should be (true)
        player1.cards.contains(Card(3, Diamonds())) should be (true)
        player2.cards should be (List(Card(2, Diamonds())))

        deck.isInstanceOf[EmptyDeck] should be (true)

        success should be (false)
        ranOut should be (true)
      }
    }
  }
}
