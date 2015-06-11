package d_m

object CardUtils {


  /**
   * Returns a standard 52 card deck
   * @return a standard 52 card deck
   */
  def standardDeck(): NonemptyDeck = NonemptyDeck((for {
    num <- 1 to 13
    suite <- List(Hearts(), Diamonds(), Clubs(), Spades())
  } yield Card(num, suite)).toList)
}
