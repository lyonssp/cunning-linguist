package grammar

//Phonemes
object Phoneme extends Enumeration {
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

case class TaggedSentence(taggedWords: Seq[(String, PartsOfSpeech.PartsOfSpeech)]) {

  def tokenized: String =
    taggedWords map { case (w, t) => t.toString ++ " " ++ w ++ " " } mkString
}
