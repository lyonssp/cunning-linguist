package cmu.pronunciation.dictionary

import org.scalatest.FunSuite

class PronunciationDictionarySuite extends FunSuite {

  test("PronunciationDictionary.fromPath can parse a subset of the CMU Pronunciation Dictionary") {
    val testSource = scala.io.Source.fromResource("cmu-pronunciation/cmudict-0.7b")
    PronunciationDictionary.fromSource(testSource).pronunciations.foreach(println)
  }
}
