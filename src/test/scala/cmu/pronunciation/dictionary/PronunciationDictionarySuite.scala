package cmu.pronunciation.dictionary

import grammar._
import org.scalatest.{FunSuite, Matchers}

class PronunciationDictionarySuite extends FunSuite with Matchers {
  val dictionary = PronunciationDictionary(
    Map(
      "AA" -> Pronunciation(
        List(
          StressedPhoneme(Phoneme.AA, Stress.Primary)
        )
      ),
      "WORD" -> Pronunciation(
        List(
          StressedPhoneme(Phoneme.W, Stress.None),
          StressedPhoneme(Phoneme.ER, Stress.Secondary),
          StressedPhoneme(Phoneme.D, Stress.None)
        )
      ),
      "ABATE" -> Pronunciation(
        List(
          StressedPhoneme(Phoneme.AH, Stress.Primary),
          StressedPhoneme(Phoneme.B, Stress.None),
          StressedPhoneme(Phoneme.EY, Stress.Secondary),
          StressedPhoneme(Phoneme.T, Stress.None)
        )
      ),
      "ABANDONMENTS" -> Pronunciation(
        List(
          StressedPhoneme(Phoneme.AH, Stress.None),
          StressedPhoneme(Phoneme.B, Stress.None),
          StressedPhoneme(Phoneme.AE, Stress.Primary),
          StressedPhoneme(Phoneme.N, Stress.None),
          StressedPhoneme(Phoneme.D, Stress.None),
          StressedPhoneme(Phoneme.AH, Stress.None),
          StressedPhoneme(Phoneme.N, Stress.None),
          StressedPhoneme(Phoneme.M, Stress.None),
          StressedPhoneme(Phoneme.AH, Stress.None),
          StressedPhoneme(Phoneme.N, Stress.None),
          StressedPhoneme(Phoneme.T, Stress.None),
          StressedPhoneme(Phoneme.S, Stress.None)
        )
      )
    )
  )

  test("PronunciationDictionary.getWord returns None for words not present") {
    assert(dictionary.getWord("notaword").isEmpty)
  }

  test("Case does not affect the word returned by getWord") {
    val word = "abate"
    assert(dictionary.getWord(word.toUpperCase).isDefined)
    assert(dictionary.getWord(word.toLowerCase).isDefined)
  }

  test("The correct phonemes are retrieved") {
    dictionary.getUnstressedPhonemes("abate").get should contain allOf(Phoneme.AH, Phoneme.B, Phoneme.EY, Phoneme.T)
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
    assert(histogram.get(Phoneme.M) == 1)
    assert(histogram.get(Phoneme.T) == 1)
    assert(histogram.get(Phoneme.S) == 1)
  }

  test("PronunciationDictionary.filter removes dictionary entries containing more than 4 phonemes") {
    val filtered = dictionary.filterByPhonemes(_.phonemes.length <= 4)
    assert(filtered.contains("word"))
    assert(filtered.contains("abate"))
    assert(!filtered.contains("abandonments"))
  }

  test("PronunciationDictionary.filter removes dictionary entries beginning with 'a'") {
    val filtered = dictionary.filterByText(_.charAt(0).toLower == 'a')
    assert(!filtered.contains("word"))
    assert(filtered.contains("abate"))
    assert(filtered.contains("abandonments"))
  }

  test("PronunciationDictionary.filter removes dictionary entries beginning with 'a' with more than 5 phonemes") {
    val filtered = dictionary.filter({
      case (w,p) => w.charAt(0).toLower != 'a' || p.phonemes.length <= 5
    })
    assert(filtered.contains("word"))
    assert(filtered.contains("abate"))
    assert(!filtered.contains("abandonments"))
  }

  test("PronunciationDictionary.filter removes words containing only vowels") {
    val filtered = dictionary.filterByText(_.exists(GrammarUtils.isConsonant))
    assert(!filtered.contains("aa"))
    assert(filtered.contains("word"))
    assert(filtered.contains("abate"))
    assert(filtered.contains("abandonments"))
  }
}