import POS.POS
import Phoneme.Phoneme
import Stress.Stress

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
  val None,
  Primry,
  Secondary = Value
}

case class StressedPhoneme(p: Phoneme, stress: Option[Stress])

case class Pronunciation(word: String, sps: Seq[StressedPhoneme])

/*
.	punctuation mark, sentence closer	.;?* 
,	punctuation mark, comma	, 
:	punctuation mark, colon	: 
(	contextual separator, left paren	( 
)
 */
object POS extends Enumeration {

  type POS = Value
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
  PRP$,
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
  WP$,
  WRB,
  PERIOD,
  COMMA,
  COLON,
  LPAREN,
  RPAREN = Value
}

case class TaggedSentence(taggedWords: Seq[(String, POS)])
