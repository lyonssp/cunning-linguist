package masc.parse

import scala.util.parsing.combinator._

object SExpParser extends RegexParsers  {

  def lparen: Parser[Char] = elem('(')
  def rparen: Parser[Char] = elem(')')
  def exp: Parser[SExp] = lparen ~> list <~ rparen
  def exps: Parser[Seq[SExp]] = rep1(exp)
  def list: Parser[SList] = rep1( (atom | exp) ) ^^ { case car +: cdr => SList(car, cdr) }
  def atom: Parser[SAtom] = """[^()\s]+""".r ^^ { SAtom(_) }

  def apply(in: String): Either[String, Seq[SExp]] = parseAll(exps, in) match {
    case Success(result, _ ) => Right(result)
    case fail: NoSuccess => Left(fail.msg)
  }

}
