package masc.parse

import java.io.File

import grammar._

import scala.util.Try

object MASCParser {

  def parseTree(treeStr: String): TaggedSentence = {
    def go(sentence: Vector[TaggedWord], tokens: Seq[String]): Vector[TaggedWord] = tokens match {
      case Seq() => sentence
      case (posStr +: parsedWord +: rest) if Try(PartsOfSpeech.withName(posStr)).isSuccess =>
        go(sentence :+ TaggedWord(Word(parsedWord), PartsOfSpeech.withName(posStr)), rest)
      case (t +: ts) => go(sentence, ts)
    }

    TaggedSentence(
      go(
        Vector(),
        treeStr
          .filterNot("()".contains(_))
          .map { c => if ("\n\t".contains(c)) ' ' else c }
          .split(' ').filterNot(_ == "").map {
          case "PRP$" => "PRPS"
          case "WP$" => "WP$"
          case "." => "PERIOD"
          case "," => "COMMA"
          case ":" => "COLON"
          case a => a
        }
      )
    )

  }

  def parseTrees(treeStrs: String): Seq[TaggedSentence] =
    treeStrs split ("""[\n]{2,}""") map parseTree

  def readAll: Seq[TaggedSentence] =
    (new File("src/main/resources/Propbank"))
      .listFiles
      .map(scala.io.Source.fromFile(_).mkString)
      .flatMap(parseTrees)

}
