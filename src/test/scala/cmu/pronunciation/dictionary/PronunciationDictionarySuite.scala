package cmu.pronunciation.dictionary

import org.scalatest.FunSuite

class PronunciationDictionarySuite extends FunSuite {
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
}
