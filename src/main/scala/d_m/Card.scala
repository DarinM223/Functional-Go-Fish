package d_m

object Suite {
  trait Suite
  case object Hearts extends Suite
  case object Spades extends Suite
  case object Diamonds extends Suite
  case object Clubs extends Suite
}

case class Card(number: Int, suite: Suite.Suite)
