import org.scalacheck.Gen
import Gen._

import grammar._
import PartsOfSpeech._

object GrammarGens {

  def genPOSTag: Gen[PartsOfSpeech] = Gen.oneOf(PartsOfSpeech.values.toList)

  def genWord: Gen[String] = nonEmptyListOf(alphaChar) map (_.mkString)

  def genSentence: Gen[Seq[String]] = nonEmptyListOf(genWord)

}
