package cmu.pronunciation.dictionary

import grammar.{Pronunciation, StressedPhoneme}

import scala.io.Source

object PronunciationDictionary {
  //Regex patterns for CMU dictionary
  val cmuComment =
  """(\s*^;;;)""".r
  val cmuDuplicateWord = """(.*)\([0-9]+\)""".r

  //method to normalize all words
  def normalizeWord: String => String = (str: String) => str.toUpperCase

  /*
  Parse Dictionary like CMU Pronunciation Dictionary from a file path
   */
  def fromSource(src: Source): PronunciationDictionary = {
    def cleanWord(rawWord: String): String = normalizeWord(
      rawWord match {
        case cmuDuplicateWord(word) => word
        case _ => rawWord
      }
    )

    def readPronunciation(line: String): (String, Pronunciation) = line.split("""\s+""").toList match {
      case word :: phonemes => (word, Pronunciation(phonemes.map(StressedPhoneme.fromString)))
      case _ => throw new RuntimeException(s"Could not parse '$line' in file ${src.descr}")
    }

    new PronunciationDictionary(
      src.getLines()
        .filter(cmuComment.findFirstMatchIn(_).isEmpty)
        .map(readPronunciation)
        .toMap
    )
  }
}

case class PronunciationDictionary(pronunciations: Map[String, Pronunciation]) {
  def getWord(word: String): Option[Pronunciation] = pronunciations.get(PronunciationDictionary.normalizeWord(word))
}