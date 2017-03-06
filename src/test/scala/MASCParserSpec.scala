import org.scalatest._
import prop._

import masc.parse.MASCParser._
import MASCParserGens._

class MASCParserSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  property("parse(taggedSentence.tokenized) == taggedSentence") {
    forAll(genTaggedSentence) { ts =>
      ts shouldBe parseTree(ts.tokenized)
    }
  }

  property("Parser is idempotent") {
    forAll(intersperseTokens) { str =>
      parseTree(str) shouldBe parseTree(parseTree(str).tokenized)
    }
  }

}
