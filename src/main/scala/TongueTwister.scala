import scala.math.log

import cats.instances.int._
import cats.instances.map._
import cats.Monoid

import grammar._
import Phoneme._
import PartsOfSpeech._

import MASCParser._
import cmu.pronunciation.dictionary._

object TongueTwister {

  def entropy(phonemeCounts: Map[Phoneme, Int]): Double = {
    val norm: Double = phonemeCounts.values.sum
    val probs = phonemeCounts.values map (_ / norm)
    probs map (p => -p * log(p)) sum
  }

  def countPhonemes(word2pronunciation: PronunciationDictionary)(words: Seq[String]): Map[Phoneme, Int] =
    Monoid[Map[Phoneme, Int]].combineAll(
      words map (word2pronunciation.getHistogram(_).getOrElse(Map[Phoneme,Int]()))
    )

  def chooseNextWord(
    posWordMap: Map[PartsOfSpeech, Seq[String]],
    word2pronunciation: PronunciationDictionary)
    (
      sentence: Seq[String],
      nextTag: PartsOfSpeech): Seq[String] = {

    val candidateWords = posWordMap(nextTag)
    candidateWords
      .map(w => (sentence :+ w, (if (sentence.contains(w)) 10.0 else 1.0)*entropy(countPhonemes(word2pronunciation)(sentence :+ w))))
      .minBy (_._2)
      ._1
  }

  def randomTemplate(templates: Seq[Seq[PartsOfSpeech]]): Seq[PartsOfSpeech] =
    templates(scala.util.Random.nextInt(templates.size))

  def randomTagged(tss: Seq[TaggedSentence]): TaggedSentence =
    tss(scala.util.Random.nextInt(tss.size))

  def randomTongueTwister: (Seq[String],Seq[String]) = {
    val folder = chooseNextWord(posWordMap, pronunciationDict) _
    val (sent, temp) = randomTagged(allTagged).taggedWords.unzip
    (sent, temp.foldLeft(Seq[String]())(folder))
  }

  val pronunciationDict = PronunciationDictionary.CMU
  val allTagged = readAll
  val allTemplates = POSTemplates(allTagged)
  val posWordMap = wordsPOS(allTagged) mapValues (_ filter (pronunciationDict.contains(_)))

}
