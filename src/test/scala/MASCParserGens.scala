import GrammarGens._
import grammar._
import org.scalacheck.Gen
import org.scalacheck.Gen._

object MASCParserGens {

  def genTaggedSentence: Gen[TaggedSentence] =
    for {
      sent <- genSentence
      tags <- listOfN(sent.size, genPOSTag)
    } yield {
      TaggedSentence(sent zip tags map { case (s, t) => TaggedWord(s, t) })
    }

  def intersperseTokens: Gen[String] =
    for {
      ts <- genTaggedSentence
      (w, t) <- (ts.taggedWords map { case TaggedWord(w, t) => (w, t) }).unzip
      maybeTok1 <- oneOf("", "TOKEN")
      maybeTok2 <- oneOf("", "TOKEN")
    } yield {
      maybeTok1 ++ t.toString ++ " " ++ w ++ " " ++ maybeTok2
    }.mkString
}
