package cmu.pronunciation.dictionary

import grammar.{Pronunciation, StressedPhoneme}

import scala.io.Source

object PronunciationDictionary {
  //Regex patterns for CMU dictionary
  val cmuComment = """(\s*^;;;)""".r
  val cmuDuplicateWord = """(.*)\([0-9]+\)""".r

  /*
  Parse Dictionary like CMU Pronunciation Dictionary from a file path
   */
  def fromSource(src: Source): PronunciationDictionary = {
    def cleanWord(rawWord: String): String = rawWord match {
      case cmuDuplicateWord(word) => word
      case _ => rawWord
    }
    def readPronunciation(line: String): Pronunciation = line.split("""\s+""").toList match {
      case head :: rest => Pronunciation(cleanWord(head), rest.map(StressedPhoneme.fromString))
      case _ => throw new RuntimeException(s"Could not parse '$line' in file ${src.descr}")
    }

    new PronunciationDictionary(
      src.getLines()
        .filter(cmuComment.findFirstMatchIn(_).isEmpty)
        .map(readPronunciation)
        .toSeq
    )
  }
}

case class PronunciationDictionary(pronunciations: Seq[Pronunciation])