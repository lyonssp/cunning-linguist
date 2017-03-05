import java.io.File
import scala.util.Try

import grammar._
import PartsOfSpeech._

object MASCParser {

  def parseTree(treeStr: String): TaggedSentence = {
    def go(
      sentence: Vector[(String, PartsOfSpeech)],
      tokens: Seq[String])
        : Vector[(String, PartsOfSpeech)] =
      tokens match {
        case Seq() => sentence
        case (posStr +: word +: rest) if Try(PartsOfSpeech.withName(posStr)).isSuccess =>
          go(sentence :+ (word, PartsOfSpeech.withName(posStr)), rest)
        case (t +: ts) => go(sentence, ts)
      }

    TaggedSentence(go(Vector(), treeStr
      .filterNot( "()".contains(_) )
      .map { c => if ("\n\t".contains(c)) ' ' else c }
      .split(' ').filterNot(_ == "").map {
      case "PRP$" => "PRPS"
      case "WP$" => "WP$"
      case "." => "PERIOD"
      case "," => "COMMA"
      case ":" => "COLON"
      case a => a
    }))
    
  }

  def parseTrees(treeStrs: String): Seq[TaggedSentence] =
    treeStrs split ("""[\n]{2,}""") map parseTree

  def readAll: Seq[TaggedSentence] = 
    (new File("src/main/resources/Propbank"))
      .listFiles
      .map(scala.io.Source.fromFile(_).mkString)
      .flatMap(parseTrees)

  def POSTemplates(tss: Seq[TaggedSentence]): Seq[Seq[PartsOfSpeech]] = tss map (_.tags)

  def wordsPOS(tss: Seq[TaggedSentence]): Map[PartsOfSpeech, Seq[String]] = {
    val tagWords: Seq[(String, PartsOfSpeech)] = tss flatMap (_.taggedWords)
    val tmp = tagWords groupBy (_._2) mapValues (vals => (vals map (_._1.toLowerCase)).distinct)
    tmp.updated(COMMA, Seq(",")).updated(PERIOD, Seq(".")).updated(COLON, Seq(":"))
  }

}
