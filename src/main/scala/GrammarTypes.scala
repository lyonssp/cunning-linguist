sealed trait Phoneme
case class AA extends Phoneme
case class AE extends Phoneme
case class AH extends Phoneme
case class AO extends Phoneme
case class AW extends Phoneme
case class AY extends Phoneme
case class B extends Phoneme
case class CH extends Phoneme
case class D extends Phoneme
case class DH extends Phoneme
case class EH extends Phoneme
case class ER extends Phoneme
case class EY extends Phoneme
case class F extends Phoneme
case class G extends Phoneme
case class HH extends Phoneme
case class IH extends Phoneme
case class IY extends Phoneme
case class JH extends Phoneme
case class K extends Phoneme
case class L extends Phoneme
case class M extends Phoneme
case class N extends Phoneme
case class NG extends Phoneme
case class OW extends Phoneme
case class OY extends Phoneme
case class P extends Phoneme
case class R extends Phoneme
case class S extends Phoneme
case class SH extends Phoneme
case class T extends Phoneme
case class TH extends Phoneme
case class UH extends Phoneme
case class UW extends Phoneme
case class V extends Phoneme
case class W extends Phoneme
case class Y extends Phoneme
case class Z extends Phoneme
case class ZH extends Phoneme

sealed trait Stress
case class None extends Stress
case class Primary extends Stress
case class Secondary extends Stress

case class StressedPhoneme(p: Phoneme, stress: Option[Stress])
case class Pronunciation(word: String, sps: Seq[StressedPhoneme])

sealed trait POS
case class CC extends POS
case class CD extends POS
case class DT extends POS
case class EX extends POS
case class FW extends POS
case class IN extends POS
case class JJ extends POS
case class JJR extends POS
case class JJS extends POS
case class LS extends POS
case class MD extends POS
case class NN extends POS
case class NNS extends POS
case class NNP extends POS
case class NNPS extends POS
case class PDT extends POS
case class POS extends POS
case class PRP extends POS
case class PRP$ extends POS
case class RB extends POS
case class RBR extends POS
case class RBS extends POS
case class RP extends POS
case class SYM extends POS
case class TO extends POS
case class UH extends POS
case class VB extends POS
case class VBZ extends POS
case class VBP extends POS
case class VBD extends POS
case class VBN extends POS
case class VBG extends POS
case class WDT extends POS
case class WP extends POS
case class WP$ extends POS
case class WRB extends POS
case class PERIOD extends POS // .
case class COMMA extends POS  // ,
case class COLON extends POS // :
case class LPAREN extends POS // (
case class RPAREN extends POS // )

case class TaggedSentence(taggedWords: Seq[(String, POS)])
