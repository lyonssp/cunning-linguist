import grammar.PartsOfSpeech._
import grammar._
import masc.parse.MASCParser._
import org.scalatest._


class MASCParserSuite extends FunSuite with Matchers {

  test("shallow example from file") {
    parseTree("( (NP-TMP (NNP December) (CD 1998)))") shouldBe
      TaggedSentence(Vector(Word("December").tag(NNP), Word("1998").tag(CD)))
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
      TaggedSentence(
        Vector(
          Word("Your").tag(PRPS),
          Word("contribution").tag(NN),
          Word("to").tag(TO),
          Word("Goodwill").tag(NNP),
          Word("will").tag(MD),
          Word("mean").tag(VB),
          Word("more").tag(JJR),
          Word("than").tag(IN),
          Word("you").tag(PRP),
          Word("may").tag(MD),
          Word("know").tag(VB),
          Word(".").tag(PERIOD)
        )
      )
  }

  test("full file parser does not exhibit exceptional behaviour") {
    parseTrees(scala.io.Source.fromResource("110CYL067.mrg").mkString)
  }

}
