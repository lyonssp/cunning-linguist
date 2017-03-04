import scala.util.Try

import grammar._

object MASCParser {

  def parseTree(treeStr: String): TaggedSentence = {
    val tokenized = treeStr
      .filterNot( "()".contains(_) )
      .map { c => if ("\n\t".contains(c)) ' ' else c }
      .split(' ').filterNot(_ == "").map {
      case "PRP$" => "PRPS"
      case "WP$" => "WP$"
      case a => a
    }

    def go(
      sentence: Vector[(String, POS.POS)],
      tokens: Seq[String])
        : Vector[(String, POS.POS)] =
      tokens match {
        case Seq() => sentence
        case (posStr +: word +: rest) if Try(POS.withName(posStr)).isSuccess =>
          go(sentence :+ (word, POS.withName(posStr)), rest)
        case (t +: ts) => go(sentence, ts)
      }
    TaggedSentence(go(Vector(), tokenized))
  }
}
