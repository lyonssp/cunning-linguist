package grammar

import grammar.PartsOfSpeech.PartsOfSpeech

//Phonemes
object Phoneme extends Enumeration {
  def isVowel(p: Phoneme): Boolean =
    Set(AA, AE, AH, AO, AW, AY, EH, ER, EY, IH, IY, OW, OY, UH, UW, V) contains p

  type Phoneme = Value
  val
  AA,
  AE,
  AH,
  AO,
  AW,
  AY,
  B,
  CH,
  D,
  DH,
  EH,
  ER,
  EY,
  F,
  G,
  HH,
  IH,
  IY,
  JH,
  K,
  L,
  M,
  N,
  NG,
  OW,
  OY,
  P,
  R,
  S,
  SH,
  T,
  TH,
  UH,
  UW,
  V,
  W,
  Y,
  Z,
  ZH = Value
}

//Phoneme stress type
object Stress extends Enumeration {
  type Stress = Value
  val
  None,
  Primary,
  Secondary = Value
}

object StressedPhoneme {
  private val regex = "([a-zA-z]+)([0-2])?".r

  def fromString(str: String): StressedPhoneme = str match {
    case regex(phonemeString, null) => StressedPhoneme(
      Phoneme.withName(phonemeString),
      Stress.None
    )
    case regex(phonemeString, stressNumber) => StressedPhoneme(
      Phoneme.withName(phonemeString),
      Stress.values.toList(stressNumber.toInt)
    )
  }
}

case class StressedPhoneme(p: Phoneme.Phoneme, stress: Stress.Stress)

case class Pronunciation(phonemes: Seq[StressedPhoneme])

/*
.	punctuation mark, sentence closer	.;?* 
,	punctuation mark, comma	, 
:	punctuation mark, colon	: 
(	contextual separator, left paren	( 
)
 */
object PartsOfSpeech extends Enumeration {
  type PartsOfSpeech = Value
  val
  CC,
  CD,
  DT,
  EX,
  FW,
  IN,
  JJ,
  JJR,
  JJS,
  LS,
  MD,
  NN,
  NNS,
  NNP,
  NNPS,
  PDT,
  POS,
  PRP,
  PRPS,
  RB,
  RBR,
  RBS,
  RP,
  SYM,
  TO,
  UH,
  VB,
  VBZ,
  VBP,
  VBD,
  VBN,
  VBG,
  WDT,
  WP,
  WPS,
  WRB,
  PERIOD,
  COMMA,
  COLON,
  LPAREN,
  RPAREN = Value
}

case class Word(raw: String) {
  def tag(partOfSpeech: PartsOfSpeech): TaggedWord = TaggedWord(this, partOfSpeech)

  def toLowerCase: Word = Word(raw.toLowerCase)

  override def toString: String = raw
}

case class SentenceTemplate(template: Seq[PartsOfSpeech]) {
  def fill(filler: (Seq[Word], PartsOfSpeech) => Seq[Word]): TaggedSentence = TaggedSentence(
    template.foldLeft(Seq[Word]())({
      case (sentenceSoFar, nextPartOfSpeech) => filler(sentenceSoFar, nextPartOfSpeech)
    }).zip(template).map({
      case (w: Word, p: PartsOfSpeech) => TaggedWord(w, p)
    })
  )
}

case class TaggedWord(word: Word, tag: PartsOfSpeech) {
  def toLowerCase: TaggedWord = TaggedWord(word.toLowerCase, tag)
}

case class TaggedSentence(taggedWords: Seq[TaggedWord]) {

  def toLower: TaggedSentence = TaggedSentence(
    taggedWords map { case TaggedWord(w, t) => TaggedWord(w.toLowerCase, t) }
  )

  def tokenized: String = taggedWords map {
    case TaggedWord(w, t) => t.toString ++ " " ++ w.toString ++ " "
  } mkString

  def template: SentenceTemplate = SentenceTemplate(taggedWords map (_.tag))
}
