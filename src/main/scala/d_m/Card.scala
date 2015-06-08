package d_m

trait Suite
case class Hearts() extends Suite
case class Spades() extends Suite
case class Diamonds() extends Suite
case class Clubs() extends Suite

case class Card(number: Int, suite: Suite)
