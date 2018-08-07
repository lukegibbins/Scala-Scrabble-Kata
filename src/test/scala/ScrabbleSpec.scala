import org.scalatest.{MustMatchers, WordSpec}

class ScrabbleSpec extends WordSpec with MustMatchers{

  val scrabble = new Scrabble
  val lowerCaseWord: String = "dog"
  val testWord: String = "DOG"
  val singleCharacterWord: String = "L"
  val sevenLetterWord: String = "JACUZZI"
  val invalidWord: String = "FDGFDHFGJHFSGJHDTHTH"

  "Scrabble" should {
    "return the correct score when a word is entered" in {
      scrabble.calcScore(testWord, None, None) mustEqual(5)
    }

    "return 0 when word is lowercase" in {
      scrabble.calcScore(lowerCaseWord, None, None) mustEqual(0)
    }

    "return 0 when the word entered does not exists in the English dictionary" in {
      scrabble.calcScore(invalidWord, None, None) mustEqual(0)
    }

    "return the correct score when a single character is entered" in {
      scrabble.calcScore(singleCharacterWord, None, None) mustEqual(1)
    }

    "return the correct score and add 50 when the word entered is 7 characters long" in {
      scrabble.calcScore(sevenLetterWord, None, None) mustEqual(84)
    }

    "return the correct score with the double letter bonus applied" in {
      scrabble.calcScore(testWord, Some(scrabble.doubleLetter), Some('D')) mustEqual(7)
    }

    "return the correct score with the triple letter bonus applied" in {
      scrabble.calcScore(testWord, Some(scrabble.tripleLetter), Some('D')) mustEqual(9)
    }

    "return the correct score with the double word bonus applied" in {
      scrabble.calcScore(testWord, Some(scrabble.doubleWord), Some('D')) mustEqual(10)
    }

    "return the correct score with the triple word bonus applied" in {
      scrabble.calcScore(testWord, Some(scrabble.tripleWord), None) mustEqual(15)
    }

    "return the correct score with the double letter applied with a 7 letter word" in {
      scrabble.calcScore(sevenLetterWord, Some(scrabble.doubleLetter), Some('I')) mustEqual(85)
    }

    "return the correct score with the double letter applied with a 7 letter word but the word doesn't contain the letter" in {
      scrabble.calcScore(sevenLetterWord, Some(scrabble.doubleLetter), Some('V')) mustEqual(84)
    }

    "return the correct score with the triple letter applied with a 7 letter word but the word doesn't contain the letter" in {
      scrabble.calcScore(sevenLetterWord, Some(scrabble.tripleLetter), Some('B')) mustEqual(84)
    }

    "return the correct score with the triple letter applied with a 7 letter word" in {
      scrabble.calcScore(sevenLetterWord, Some(scrabble.tripleLetter), Some('I')) mustEqual(86)
    }

    "return the correct score with the double word applied with a 7 letter word" in {
      scrabble.calcScore(sevenLetterWord, Some(scrabble.doubleWord), None) mustEqual(168)
    }
  }

}
