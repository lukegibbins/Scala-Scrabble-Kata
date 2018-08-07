class Scrabble {

  val dictionary: Set[String] = scala.io.Source.fromFile("/usr/share/dict/british-english").getLines.map(x => x.toUpperCase).toSet
  val letters = Map(
      "E" -> 1, "A" -> 1, "I" -> 1, "O" -> 1,
      "N" -> 1, "R" -> 1, "T" -> 1, "L" -> 1,
      "S" -> 1, "U" -> 1, "D" -> 2, "G" -> 2,
      "B" -> 3, "C" -> 3, "M" -> 3, "P" -> 3,
      "F" -> 4, "H" -> 4, "V" -> 4, "W" -> 4,
      "Y" -> 4, "K" -> 5, "J" -> 8, "X" -> 8,
      "Q" -> 10, "Z" -> 10
    )

    trait Bonus
    case object doubleLetter extends Bonus
    case object tripleLetter extends Bonus
    case object doubleWord extends Bonus
    case object tripleWord extends Bonus

    def calcScore(word: String, bonus: Option[Bonus], bonusLetter: Option[Char]): Int = {
    if(dictionary.contains(word)) {

        val choiceOfLetter: Int = if(word.contains(bonusLetter.getOrElse(0))) letters(bonusLetter.get.toString) else 0
        val sevenLetterBonus = if(word.length == 7) 50 else 0
        val list: Seq[Int] = for(l <- word) yield letters(l.toString)
        val sumOfList: Int = list.sum + sevenLetterBonus

        bonus.getOrElse(None) match {
            case dl:doubleLetter.type => ((choiceOfLetter * 2) - choiceOfLetter) + sumOfList
            case tl:tripleLetter.type => ((choiceOfLetter * 3) - choiceOfLetter) + sumOfList
            case dw:doubleWord.type => sumOfList * 2
            case tw:tripleWord.type => sumOfList * 3
            case None => sumOfList
        }
    } else 0
  }

}
