package cmu.pronunciation.dictionary

import grammar.{Pronunciation, StressedPhoneme}

import scala.io.Source

object PronunciationDictionary {
  /*
  Parse Dictionary like CMU Pronunciation Dictionary from a file path
   */
  def fromPath(path: String): PronunciationDictionary = {
    def readPronunciation(line: String): Pronunciation = line.split(' ').toList match {
      case head :: rest => Pronunciation(head, rest.map(StressedPhoneme.fromString))
      case _ => throw new RuntimeException(s"Could not parse '$line' in file $path")
    }

    new PronunciationDictionary(
      Source.fromFile(path).getLines().map(readPronunciation).toSeq
    )
  }
}

case class PronunciationDictionary(pronunciations: Seq[Pronunciation])