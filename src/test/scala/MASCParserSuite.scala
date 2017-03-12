import grammar.PartsOfSpeech._
import grammar._
import masc.parse._
import masc.parse.MASCParser._
import org.scalatest._


class MASCParserSuite extends FunSuite with Matchers {

  test("shallow example from file") {
    parseAll(taggedSentence, "( (NP-TMP (NNP December) (CD 1998)))").get shouldBe
    TaggedSentence(Vector(Word("December").tag(NNP), Word("1998").tag(CD)))
  }

  test("Deeper example from file with newlines") {
    parseAll(taggedSentence,
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
      """.stripMargin).get shouldBe
      TaggedSentence(
        Vector(
          Word("Your").tag(PRP$),
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
    assert(parseAll(taggedSentences, scala.io.Source.fromResource("110CYL067.mrg").mkString).successful)
  }

}
