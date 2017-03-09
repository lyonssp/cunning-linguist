import org.scalacheck.Gen
import Gen._

import grammar._
import PartsOfSpeech._

object GrammarGens {

  def genPOSTag: Gen[PartsOfSpeech] = Gen.oneOf(PartsOfSpeech.values.toList)

  def genWord: Gen[Word] = nonEmptyListOf(alphaChar) map (chars => Word(chars.mkString))

  def genSentence: Gen[Seq[Word]] = nonEmptyListOf(genWord)

}
