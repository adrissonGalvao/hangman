package Hangman

object Hangman extends App {

  val word: String = "casa"

  val hangmanGame = HangmanGame(word)

  //hangmanGame.askLyric()

  println(hangmanGame.getOccurrence('a'))
}

case class HangmanGame(word: String) {

  val scanner = new java.util.Scanner(System.in)

  def askLyric(): Option[Char] = {
    try {
      print("Digite um letra: ")
      val attent: Char = scanner.nextLine().charAt(0)
      Some(attent)
    } catch {
      case _: Throwable => None
    }
  }

  def getOccurrence(character: Char): Option[IndexedSeq[Int]] = {
    val occurrence = word.zipWithIndex filter {
      case (characterWord, _) => characterWord == character
    } map { case (_, index) => index }
    if (occurrence.length > 0)
      Some(occurrence)
    else
      None
  }
}
