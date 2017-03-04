import Phoneme.Phoneme
import Stress.Stress

//Phonemes
object Phoneme {

  sealed trait Phoneme

  object AA extends Phoneme

  object AE extends Phoneme

  object AH extends Phoneme

  object AO extends Phoneme

  object AW extends Phoneme

  object AY extends Phoneme

  object B extends Phoneme

  object CH extends Phoneme

  object D extends Phoneme

  object DH extends Phoneme

  object EH extends Phoneme

  object ER extends Phoneme

  object EY extends Phoneme

  object F extends Phoneme

  object G extends Phoneme

  object HH extends Phoneme

  object IH extends Phoneme

  object IY extends Phoneme

  object JH extends Phoneme

  object K extends Phoneme

  object L extends Phoneme

  object M extends Phoneme

  object N extends Phoneme

  object NG extends Phoneme

  object OW extends Phoneme

  object OY extends Phoneme

  object P extends Phoneme

  object R extends Phoneme

  object S extends Phoneme

  object SH extends Phoneme

  object T extends Phoneme

  object TH extends Phoneme

  object UH extends Phoneme

  object UW extends Phoneme

  object V extends Phoneme

  object W extends Phoneme

  object Y extends Phoneme

  object Z extends Phoneme

  object ZH extends Phoneme

}

//Phoneme stress type
object Stress {

  sealed trait Stress

  object None extends Stress

  object Primary extends Stress

  object Secondary extends Stress

}

case class StressedPhoneme(p: Phoneme, stress: Option[Stress])
case class Pronunciation(word: String, sps: Seq[StressedPhoneme])

//Parts of speech
object POS {

  sealed trait POS

  object CC extends POS

  object CD extends POS

  object DT extends POS

  object EX extends POS

  object FW extends POS

  object IN extends POS

  object JJ extends POS

  object JJR extends POS

  object JJS extends POS

  object LS extends POS

  object MD extends POS

  object NN extends POS

  object NNS extends POS

  object NNP extends POS

  object NNPS extends POS

  object PDT extends POS

  object POS extends POS

  object PRP extends POS

  object PRP$ extends POS

  object RB extends POS

  object RBR extends POS

  object RBS extends POS

  object RP extends POS

  object SYM extends POS

  object TO extends POS

  object UH extends POS

  object VB extends POS

  object VBZ extends POS

  object VBP extends POS

  object VBD extends POS

  object VBN extends POS

  object VBG extends POS

  object WDT extends POS

  object WP extends POS

  object WP$ extends POS

  object WRB extends POS

}
/*
.	punctuation mark, sentence closer	.;?* 
,	punctuation mark, comma	, 
:	punctuation mark, colon	: 
(	contextual separator, left paren	( 
)
 */
