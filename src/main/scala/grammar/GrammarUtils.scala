package grammar

import PartsOfSpeech._

object GrammarUtils {
  def POSTemplates(tss: Seq[TaggedSentence]): Seq[Seq[PartsOfSpeech]] = tss map (_.tags)

  def wordsPOS(tss: Seq[TaggedSentence]): Map[PartsOfSpeech, Seq[String]] = {
    val tagWords: Seq[(String, PartsOfSpeech)] = tss flatMap (_.taggedWords)
    val tmp = tagWords groupBy (_._2) mapValues (vals => (vals map (_._1.toLowerCase)).distinct)
    tmp.updated(COMMA, Seq(",")).updated(PERIOD, Seq(".")).updated(COLON, Seq(":"))
  }

}
