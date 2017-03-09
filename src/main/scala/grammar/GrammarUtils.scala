package grammar

import PartsOfSpeech._

object GrammarUtils {
  def POSTemplates(tss: Seq[TaggedSentence]): Seq[SentenceTemplate] = tss map (_.template)

  def wordsPOS(tss: Seq[TaggedSentence]): Map[PartsOfSpeech, Seq[Word]] = {
    val tagWords: Seq[TaggedWord] = tss flatMap (_.taggedWords)
    val tmp = tagWords groupBy (_.tag) mapValues (vals => (vals map (_.word.toLowerCase)).distinct)
    tmp.updated(
      COMMA, Seq(Word(","))
    ).updated(
      PERIOD, Seq(Word("."))
    ).updated(
      COLON, Seq(Word(":"))
    )
  }

}
