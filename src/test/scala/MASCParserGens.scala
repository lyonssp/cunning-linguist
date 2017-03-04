import org.scalacheck.Gen
import Gen._

import grammar._
import PartsOfSpeech._

import GrammarGens._

object MASCParserGens {

  def genTaggedSentence: Gen[TaggedSentence] =
    for {
      sent <- genSentence
      tags <- listOfN(sent.size, genPOSTag)
    } yield {
      TaggedSentence(sent zip tags)
    }

  def intersperseTokens: Gen[String] =
    for {
      ts <- genTaggedSentence
      (w, t) <- ts map (_.taggedWords.unzip)
      maybeTok1 <- oneOf("", "TOKEN")
      maybeTok2 <- oneOf("", "TOKEN")
    } yield {
      maybeTok1 ++ t.toString ++ " " ++ w ++ " " ++ maybeTok2
    }.mkString
}
