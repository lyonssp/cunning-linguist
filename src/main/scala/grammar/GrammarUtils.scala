package grammar

import PartsOfSpeech._

object GrammarUtils {
  def POSTemplates(tss: Seq[TaggedSentence]): Seq[Seq[PartsOfSpeech]] = tss map (_.tags)

  def wordsPOS(tss: Seq[TaggedSentence]): Map[PartsOfSpeech, Seq[String]] = {
    val tagWords: Seq[TaggedWord] = tss flatMap (_.taggedWords)
    val tmp = tagWords groupBy (_.tag) mapValues (vals => (vals map (_.raw.toLowerCase)).distinct)
    tmp.updated(COMMA, Seq(",")).updated(PERIOD, Seq(".")).updated(COLON, Seq(":"))
  }

}
