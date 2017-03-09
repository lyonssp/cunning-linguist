package cmu.pronunciation.dictionary

import grammar.Phoneme.Phoneme
import grammar.{Pronunciation, StressedPhoneme, Word}

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
        .updated(",", Pronunciation(Seq()))
        .updated(".", Pronunciation(Seq()))
        .updated(":", Pronunciation(Seq()))
    )
  }

  def CMU: PronunciationDictionary = fromSource(scala.io.Source.fromResource("cmu-pronunciation/cmudict-0.7b"))
}

//TODO would prefer not to duplicate operations for Word and String
case class PronunciationDictionary(pronunciations: Map[String, Pronunciation]) {
  def contains(word: Word): Boolean = this.contains(word.raw)
  def contains(word: String): Boolean = pronunciations.contains(
    PronunciationDictionary.normalizeWord(word)
  )

  def getWord(word: Word): Option[Pronunciation] = this.getWord(word.raw)
  def getWord(rawWord: String): Option[Pronunciation] = pronunciations.get(
    PronunciationDictionary.normalizeWord(rawWord)
  )

  def getPhonemes(word: Word): Option[Seq[StressedPhoneme]] = this.getPhonemes(word.raw)
  def getPhonemes(word: String): Option[Seq[StressedPhoneme]] = this.getWord(word).map(_.phonemes)

  def getUnstressedPhonemes(word: Word): Option[Seq[Phoneme]] = this.getUnstressedPhonemes(word.raw)
  def getUnstressedPhonemes(word: String): Option[Seq[Phoneme]] = this.getPhonemes(word).map(_.map(_.p))

  def getHistogram(word: Word): Option[Map[Phoneme, Int]] = this.getHistogram(word.raw)
  def getHistogram(word: String): Option[Map[Phoneme, Int]] = this.getUnstressedPhonemes(word).map {
    phonemes => phonemes.groupBy((p: Phoneme) => p).mapValues(_.length)
  }
}
