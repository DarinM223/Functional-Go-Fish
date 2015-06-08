package d_m

trait Suite
case class Hearts() extends Suite
case class Spades() extends Suite
case class Diamonds() extends Suite
case class Clubs() extends Suite

case class Card(number: Int, suite: Suite)

trait Deck {
  def popTopCard(): (Option[Card], Deck)
  def shuffle(): Deck
}

case class EmptyDeck() extends Deck {
  def popTopCard(): (Option[Card], Deck) = (None, EmptyDeck())
  def shuffle(): Deck = EmptyDeck()
}

case class NonemptyDeck(deck: List[Card]) extends Deck {
  def popTopCard(): (Option[Card], Deck) = deck match {
    case top::List() => (Some(top), EmptyDeck())
    case top::rest => (Some(top), NonemptyDeck(rest))
  }
  def shuffle(): Deck = NonemptyDeck(util.Random.shuffle(deck.toSeq).toList)
}

trait Player {
  def query(rank: Int, player: Player, deck: Deck): (Player, Player, Deck) = if (player.hasCard(rank))
    (addCard(player.cards(player.cards.indexWhere(_.number == rank))), player.removeCard(rank), deck)
  else {
    val (optionCard, newDeck) = deck.popTopCard()
    val card = optionCard.getOrElse(Card(1, Hearts())) // TODO: fix sample code

    (addCard(player.cards(player.cards.indexWhere(_.number == rank))), player, newDeck)
  }

  val piles: Int
  val cards: Vector[Card]

  def hasCard(rank: Int): Boolean
  def removeCard(rank: Int): Player
  def addCard(card: Card): Player
}

object CardUtils {
  def removeBooks(player: Player): Player = {
    val map = player.cards.foldLeft(Map[Int, Int]())((map, card) => map.get(card.number) match {
      case Some(value) => map.updated(card.number, value + 1)
      case None => map + (card.number -> 1)
    })

    val newCards = player.cards.filter((card) => map.get(card.number) match {
      case Some(num) => num < 4
      case None => true
    })

    player match {
      case PersonPlayer(name, cards, piles) => PersonPlayer(name, newCards, piles)
      case BotPlayer(name, cards, piles) => BotPlayer(name, newCards, piles)
    }
  }
}

case class PersonPlayer(name: String, cards: Vector[Card], piles: Int) extends Player {
  def hasCard(rank: Int): Boolean = cards.exists(_.number == rank)

  def removeCard(rank: Int): Player = if (hasCard(rank))
    PersonPlayer(name, cards.patch(cards.indexWhere(_.number == rank), Nil, 1), piles)
  else
    this

  def addCard(card: Card): Player = CardUtils.removeBooks(PersonPlayer(name, cards :+ card, piles))
}

case class BotPlayer(name: String, cards: Vector[Card], piles: Int) extends Player {
  def hasCard(rank: Int): Boolean = cards.exists(_.number == rank)
  def removeCard(rank: Int): Player = if (hasCard(rank))
    BotPlayer(name, cards.patch(cards.indexWhere(_.number == rank), Nil, 1), piles)
  else
    this

  def addCard(card: Card): Player = CardUtils.removeBooks(BotPlayer(name, cards :+ card, piles))
}

/**
 * Created by darin on 6/8/15.
 */
object main {
  def main (args: Array[String]): Unit = {
    var player = PersonPlayer("darin", Vector[Card](), 0)
    player = player.addCard(Card(1, Hearts())) match {
      case p: PersonPlayer => p
    }

    println(player.cards)

    player = player.addCard(Card(1, Hearts())) match {
      case p: PersonPlayer => p
    }

    println(player.cards)

    player = player.addCard(Card(1, Hearts())) match {
      case p: PersonPlayer => p
    }

    println(player.cards)

    player = player.addCard(Card(1, Hearts())) match {
      case p: PersonPlayer => p
    }

    println(player.cards)
  }
}
