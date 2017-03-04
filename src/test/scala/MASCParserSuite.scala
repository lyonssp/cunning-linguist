import org.scalatest._
import Matchers._

import MASCParser._
import POS._

class MASCParserSuite extends FunSuite with Matchers{

  test ("shallow example from file") {
    parseTree("( (NP-TMP (NNP December) (CD 1998)))") shouldBe
    TaggedSentence(Vector(("December", NNP), ("1998", CD) ))
  }

  test ("Deeper example from file with newlines") {
    parseTree("""
    ( (S (NP-SBJ (NP (PRP$ Your) (NN contribution))
	     (PP (TO to)
		 (NP (NNP Goodwill))))
     (VP (MD will)
	 (VP (VB mean)
	     (NP (NP (JJR more))
		 (SBAR (IN than)
		       (S (NP-SBJ (PRP you))
			  (VP (MD may)
			      (VP (VB know))))))))
     (. .)))
    """.stripMargin) shouldBe
    TaggedSentence(Vector(
      ("Your", PRPS), ("contribution", NN), ("to", TO),
      ("Goodwill", NNP), ("will", MD), ("mean", VB),
      ("more", JJR), ("than", IN), ("you", PRP), ("may", MD),
      ("know", VB)))
  }
}
