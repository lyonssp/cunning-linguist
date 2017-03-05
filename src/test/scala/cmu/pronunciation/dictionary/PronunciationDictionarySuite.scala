package cmu.pronunciation.dictionary

import grammar.Phoneme
import org.scalatest.{FunSuite, Matchers}

class PronunciationDictionarySuite extends FunSuite with Matchers {
  val testSource = scala.io.Source.fromResource("cmu-pronunciation/cmudict-0.7b-reduced")
  val dictionary = PronunciationDictionary.fromSource(testSource)

  test("PronunciationDictionary.getWord returns None for words not present") {
    assert(dictionary.getWord("notaword").isEmpty)
  }

  test("Case does not affect the word returned by getWord") {
    val word = "abate"
    assert(dictionary.getWord(word.toUpperCase).isDefined)
    assert(dictionary.getWord(word.toLowerCase).isDefined)
  }

  test("The correct phonemes are retrieved") {
    dictionary.getUnstressedPhonemes("abate").get should contain allOf(Phoneme.AH,Phoneme.B,Phoneme.EY,Phoneme.T)
  }

  test("Histograms are correctly generated") {
    val histogram = dictionary.getHistogram("abate")
    assert(histogram.get(Phoneme.AH) == 1)
    assert(histogram.get(Phoneme.B) == 1)
    assert(histogram.get(Phoneme.EY) == 1)
    assert(histogram.get(Phoneme.T) == 1)
  }

  test("Histograms are correctly generated with duplicate phonemes") {
    val histogram = dictionary.getHistogram("abandonments")
    assert(histogram.get(Phoneme.AH) == 3)
    assert(histogram.get(Phoneme.B) == 1)
    assert(histogram.get(Phoneme.AE) == 1)
    assert(histogram.get(Phoneme.N) == 3)
    assert(histogram.get(Phoneme.D) == 1)
    assert(histogram.get(Phoneme.T) == 1)
    assert(histogram.get(Phoneme.S) == 1)
  }

}
