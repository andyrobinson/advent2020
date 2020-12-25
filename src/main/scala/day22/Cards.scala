package day22

object Cards {
  
  type Decks = (List[Int],List[Int])
  
  def score(cards: List[Int]):Int = {
    cards.reverse.zipWithIndex.foldLeft(0){ case (acc, (card, i)) => acc + card * (i+1) }
  }

  def recursiveCombat(decks: Decks):(String, Decks, Int) = {
    val result = game(1, decks)
    if (result._1.isEmpty) ("Player 2", result, score(result._2)) else ("Player 1", result, score(result._1))
  }

  def game(number: Int, decks: Decks, previousHands: List[Decks] = List.empty[Decks]): Decks = {
    if (previousHands.contains(decks)) (decks._1, List.empty[Int]) // loop detect
    else decks match {
      case (Nil,Nil) => throw new RuntimeException("unexpected draw!")
      case (a,b) if (a.isEmpty || b.isEmpty) => (a,b)
      case (a,b) if (a.head >= a.size || b.head >= b.size) => {
        val roundResult = playRound((a, b))
        game(number, roundResult, decks::previousHands)
      }
      case (hd1::tl1, hd2::tl2) => {
        val subGame = game(number * 10 + 1, (tl1.take(hd1),tl2.take(hd2)))
        if (subGame._1.isEmpty)
          game(number, (tl1, tl2 :+ hd2 :+ hd1), decks::previousHands)
        else
          game(number, (tl1 :+ hd1 :+ hd2,tl2), decks::previousHands)
      } 
    }
  }

  def combat(decks:Decks):Decks =
    if (decks._1.isEmpty || decks._2.isEmpty) decks
    else combat(playRound(decks))

  def playRound(decks: Decks): Decks = decks match {
    case (hd1::tl1,hd2::tl2) if hd1 > hd2 => (tl1 :+ hd1 :+ hd2,tl2)
    case (hd1::tl1,hd2::tl2) if hd1 < hd2 => (tl1,tl2 :+ hd2 :+ hd1)
    case _ => throw new RuntimeException ("Cards not expected to be equal!" + decks)
  }

  def fromInput(input: List[String]):Decks = {
    val fromFirstDeck=input.dropWhile(_ != "Player 1:").tail
    val firstDeck = fromFirstDeck.takeWhile(_.nonEmpty)
    val secondDeck=fromFirstDeck.dropWhile((_ != "Player 2:")).tail

    (firstDeck.flatMap(_.toIntOption),secondDeck.flatMap(_.toIntOption))
  }
}
