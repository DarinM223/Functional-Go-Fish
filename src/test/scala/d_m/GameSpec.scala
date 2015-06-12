package d_m

import org.scalatest._

class GameSpec extends FlatSpec with Matchers {
  "Game's query" should "update all values and return the next game" in {
    val game = Game(CardUtils.standardDeck(), Map[Int, Boolean](), Map[Int, List[Log]](), Map[String, Player](
      "test" -> PersonPlayer("test", List(Card(1, Hearts()), Card(2, Spades()), Card(3, Diamonds()), Card(3, Hearts())), 0),
      "test2" -> PersonPlayer("test2", List(Card(3, Spades()), Card(4, Clubs()), Card(3, Clubs()), Card(8, Hearts())), 0)
    ), "test", "test2", false)

    game.query("test", "test2", "test", 3) match {
      case (successful, Game(newDeck, newDiscardPile, newLogs, newPlayers, newCurrentPlayer, newNextPlayer, won)) => {
        newCurrentPlayer should be ("test2")
        newNextPlayer should be ("test")
        won should be (false)

        newDeck match {
          case d: NonemptyDeck => d.deck.length should be (52)
        }

        newPlayers.count(_ => true) should be (2)
        newPlayers("test") match {
          case PersonPlayer(_, cards, piles) => {
            println(cards)
            cards.length should be (2)
            cards.contains(Card(1, Hearts())) should be (true)
            cards.contains(Card(2, Spades())) should be (true)

            piles should be (1)
          }
        }
        newPlayers("test2") match {
          case PersonPlayer(_, cards, piles) => {
            cards.length should be (2)
            cards.contains(Card(4, Clubs())) should be (true)
            cards.contains(Card(8, Hearts())) should be (true)

            piles should be (0)
          }
        }

        newDiscardPile should be (Map[Int, Boolean](3 -> true))
        newLogs should be (Map(3 -> List(Log(newPlayers("test"), newPlayers("test2"), true))))
        successful should be (true)
      }
    }
  }
}
