import grammar.PartsOfSpeech._
import grammar._
import masc.parse.MASCParser._
import org.scalatest._


class MASCParserSuite extends FunSuite with Matchers {

  test("shallow example from file") {
    parseTree("( (NP-TMP (NNP December) (CD 1998)))") shouldBe
      TaggedSentence(Vector(TaggedWord("December", NNP), TaggedWord("1998", CD)))
  }

  test("Deeper example from file with newlines") {
    parseTree(
      """
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
        TaggedWord("Your", PRPS),
        TaggedWord("contribution", NN),
        TaggedWord("to", TO),
        TaggedWord("Goodwill", NNP),
        TaggedWord("will", MD),
        TaggedWord("mean", VB),
        TaggedWord("more", JJR),
        TaggedWord("than", IN),
        TaggedWord("you", PRP),
        TaggedWord("may", MD),
        TaggedWord("know", VB),
        TaggedWord("PERIOD", PERIOD)))
  }

  test("full file parser does not exhibit exceptional behaviour") {
    parseTrees(scala.io.Source.fromResource("110CYL067.mrg").mkString)
  }

}
