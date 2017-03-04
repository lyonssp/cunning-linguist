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
      case a => a
    }))
    
  }

  def parseTrees(treeStrs: String) =
    treeStrs split ("""[\n]{2,}""") map parseTree
  
}
