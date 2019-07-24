package Hangman

import com.sun.tools.javac.util.Position

import scala.util.Try

object Hangman extends App {

  val word: String = "casa"
  val tryWord: String = "_" * word.length()

  println(HangmanGame.game(0, word, tryWord))

}

object HangmanGame {

  val scanner = new java.util.Scanner(System.in)

  def askLyric(): Either[String, Char] = {
    print("Digite uma letra: ")
    Try(scanner.nextLine().charAt(0))
      .toEither
      .fold(_ => Left("Caracter invalido"), Right.apply)
  }

  def getOccurrence(word: String, character: Char): Either[String, IndexedSeq[Int]] = {
    val occurrence = word
      .zipWithIndex
      .filter {
        case (characterWord, _) => characterWord == character
      }
      .map {
        case (_, index) => index
      }

    if (occurrence.nonEmpty)
      Right(occurrence)
    else
      Left("Caracter não Encotrado")
  }

  def createWord(tryCharacter: Char, positions: IndexedSeq[Int], wordUser: String): String = {
    val tWord = wordUser.zipWithIndex.map {
      case (character, index) => if (positions.contains(index)) tryCharacter else character
    }
    tWord.mkString
  }

  def game(erros: Int, word: String, tryWord: String): String = {
    println(erros, tryWord)
    if (erros > 6 || word.equals(tryWord))
      "Fim de Jogo"
    else {
      val newTryWord = for {
        lyric <- askLyric()
        ocorrenceWord <- getOccurrence(word, lyric)

      } yield createWord(lyric, ocorrenceWord, tryWord)

      newTryWord.fold(_ => game(erros + 1, word, tryWord), newString => game(erros, word, newString))

    }
  }
}


