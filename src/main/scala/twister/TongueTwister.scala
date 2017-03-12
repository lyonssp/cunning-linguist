package twister

import cats.Monoid
import cats.instances.int._
import cats.instances.map._
import cmu.pronunciation.dictionary._
import grammar.GrammarUtils._
import grammar.PartsOfSpeech._
import grammar.Phoneme._
import grammar._
import masc.parse.MASCParser._
import org.scalacheck.Gen
import org.scalacheck.Gen._

import scala.math.log

object TongueTwister {

  def entropy(phonemeCounts: Map[Phoneme, Int]): Double = {
    val norm: Double = phonemeCounts.values.sum
    val probs = phonemeCounts.values map (_ / norm)
    probs.map(p => -p * log(p)).sum
  }

  def countPhonemes(word2pronunciation: PronunciationDictionary)(words: Seq[Word]): Map[Phoneme, Int] =
    Monoid[Map[Phoneme, Int]].combineAll(
      words.map(word2pronunciation.getHistogram(_)).flatten
    )

  def evaluateCandidateWord(word2pron: PronunciationDictionary, history: Seq[Word])(candidate: Word): (Seq[Word], Double) = {
    val sentence = history :+ candidate
    val ent = entropy(countPhonemes(word2pron)(sentence))
    val unstressed = word2pron.getUnstressedPhonemes(candidate) getOrElse (Seq[Phoneme]())
    val nPhonemes = unstressed.size
    val vowelCounts = unstressed count isVowel
    val vowelRatio = vowelCounts.toDouble / nPhonemes

    val repetitionPenalty = if (history contains candidate) 10.0 else 1.0
    val vowelPenalty = if (vowelRatio >= 0.25) 10.0 else 1.0
    val penalty = List(repetitionPenalty, vowelPenalty).product
    (sentence, penalty * ent)
  }


  def chooseNextWord(posWordMap: Map[PartsOfSpeech, Seq[Word]], word2pron: PronunciationDictionary)
                    (context: Seq[Word])
                    (nextTag: PartsOfSpeech): Seq[Word] = {
    val candidateWords = posWordMap(nextTag)
    candidateWords
      .map(evaluateCandidateWord(word2pron, context))
      .minBy(_._2)
      ._1
  }

  def randomTemplate(templates: Seq[Seq[PartsOfSpeech]]): Gen[Seq[PartsOfSpeech]] =
    oneOf(templates)

  def randomTagged(tss: Seq[TaggedSentence]): Gen[TaggedSentence] =
    oneOf(tss)

  def templateToTwister(sentenceTemplate: SentenceTemplate): TaggedSentence = {
    val folder = chooseNextWord(posWordMap, pronunciationDict)(_: Seq[Word])(_: PartsOfSpeech)
    sentenceTemplate.fill(folder)
  }

  def tongueTwisterWithHistory: Gen[(TaggedSentence, TaggedSentence)] =
    for { sentenceSource <- randomTagged(allTagged).filter(!_.isEmpty) }
    yield { (sentenceSource, templateToTwister(sentenceSource.template)) }

  val pronunciationDict = PronunciationDictionary.CMU
  val allTagged = readAllTaggedSents
  val allTemplates = POSTemplates(allTagged)
  val posWordMap = wordsPOS(allTagged) mapValues (_ filter pronunciationDict.contains)

}
