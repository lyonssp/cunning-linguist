package masc.parse

import grammar._
import java.io.File
import scala.util.Try

object MASCParser extends SExpParser {

  def taggedSentence: Parser[TaggedSentence] = {

    def sexp2taggedSent(s: SExp): TaggedSentence = {
      TaggedSentence(s.getLeaves.map {
        case (pos, word) => Try((PartsOfSpeech.withName(pos), word))
      } collect { // silently throws away unrecognized leaves for now
        case scala.util.Success((p, w)) => TaggedWord(Word(w), p)
      })
    }

    exp map sexp2taggedSent
  }

  def taggedSentences: Parser[Seq[TaggedSentence]] = rep(taggedSentence)

  // prints failed parse messages
  def readAllTaggedSents: Seq[TaggedSentence] =
    (new File("src/main/resources/Propbank"))
      .listFiles
      .map {
      fn => (fn, parseAll(taggedSentences, scala.io.Source.fromFile(fn).mkString))
    }.collect {
      case (fn, Success(ts, _)) => ts
      case (fn, e: NoSuccess) => { println(fn); println(e); Seq[TaggedSentence]() }
    }.toSeq.flatten
}
