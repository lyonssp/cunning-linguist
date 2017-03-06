import scala.math.log

import cats.instances.int._
import cats.instances.map._
import cats.Monoid

import grammar._
import GrammarUtils._
import Phoneme._
import PartsOfSpeech._

import masc.parse.MASCParser._
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

  def evaluateCandidateWord(word2pron: PronunciationDictionary, history: Seq[String])(candidate: String): (Seq[String], Double) = {
    val sentence = history :+ candidate
    val ent = entropy(countPhonemes(word2pron)(sentence))
    val unstressed = word2pron.getUnstressedPhonemes(candidate) getOrElse (Seq[Phoneme]())
    val nPhonemes = unstressed size
    val vowelCounts = unstressed count isVowel
    val vowelRatio = vowelCounts.toDouble / nPhonemes
    
    val repetitionPenalty = if (history contains candidate) 10.0 else 1.0
    val vowelPenalty = if (vowelRatio >= 0.25) 10.0 else 1.0
    val penalty = List(repetitionPenalty, vowelPenalty).reduce(_*_)
    (sentence, penalty*ent)
  }


  def chooseNextWord(posWordMap: Map[PartsOfSpeech, Seq[String]], word2pron: PronunciationDictionary)(sentence: Seq[String], nextTag: PartsOfSpeech): Seq[String] = {

    val candidateWords = posWordMap(nextTag)
    candidateWords
      .map(evaluateCandidateWord(word2pron, sentence))
      .minBy(_._2)
      ._1
  }

  def randomTemplate(templates: Seq[Seq[PartsOfSpeech]]): Seq[PartsOfSpeech] =
    templates(scala.util.Random.nextInt(templates.size))

  def randomTagged(tss: Seq[TaggedSentence]): TaggedSentence =
    tss(scala.util.Random.nextInt(tss.size))

  def randomTongueTwister: (Seq[String],Seq[String]) = {
    val folder = chooseNextWord(posWordMap, pronunciationDict) _
    val (sentence, temp) = randomTagged(allTagged).taggedWords.map({
      case TaggedWord(raw, tag) => (raw,tag)
    }).unzip
    (sentence, temp.foldLeft(Seq[String]())(folder))
  }

  val pronunciationDict = PronunciationDictionary.CMU
  val allTagged = readAll
  val allTemplates = POSTemplates(allTagged)
  val posWordMap = wordsPOS(allTagged) mapValues (_ filter pronunciationDict.contains)

}
