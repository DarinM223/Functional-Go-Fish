package d_m

class Player(val name: String) {
  private var _cards: List[Card] = List()
  private var _piles: Int = 0

  def cards = _cards
  def piles = _piles

  def query(rank: Int, player: Player, deck: Deck): Deck =
    if (player.hasCard(rank)) {
      addCard(player.cards(player.cards.indexWhere(_.number == rank)))
      player.removeCard(rank)
      deck
    } else {
      val (optionCard, _deck) = deck.popTopCard()

      // If deck is empty, create a new deck and get the top card
      val (card, newDeck) = optionCard match {
        case Some(card) => (Some(card), _deck)
        case None => {
          val newDeck = CardUtils.standardDeck()
          newDeck.popTopCard()
        }
      }

      addCard(card.getOrElse(Card(1, Hearts())))
      newDeck
    }

  def hasCard(rank: Int): Boolean = _cards.exists(_.number == rank)

  /**
   * Removes books (4 cards with the same number) from the players hand
   * the order of cards in the resulting players hand is not guaranteed to be the same as before
   */
  def removeBooks() = {
    val map = cards.foldLeft(Map[Int, Int]())((map, card) => map.get(card.number) match {
      case Some(value) => map.updated(card.number, value + 1)
      case None => map + (card.number -> 1)
    })

    val (newCards, _) = cards.foldLeft((List[Card](), map))((tuple, card) => tuple match {
      case (cards, map) => map.get(card.number) match {
        case Some(num) => num match {
          case num if num < 4 => (card::cards, map)
          case num if num % 4 == 0 => (cards, map)
          case _ => (card::cards, map.updated(card.number, num - 1))
        }
        case None => (card::cards, map)
      }
    })

    val addAmount: Int = map.foldLeft(0)((count, keyValue) => count + Math.floorDiv(keyValue._2, 4))

    _cards = newCards
    _piles += addAmount
  }

  def removeCard(rank: Int) =
    if (hasCard(rank))
      _cards = _cards.patch(_cards.indexWhere(_.number == rank), Nil, 1)

  def addCard(card: Card) = {
    _cards = card::_cards
    removeBooks()
  }
}