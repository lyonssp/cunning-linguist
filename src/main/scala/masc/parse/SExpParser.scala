package masc.parse

import scala.util.parsing.combinator._

class SExpParser extends RegexParsers {

  def exp: Parser[SExp] = "(" ~> list <~ ")"
  def list: Parser[SList] = rep1((atom | exp)) ^^ { case car +: cdr => SList(car, cdr) }
  def atom: Parser[SAtom] = """[^()\s]+""".r ^^ { SAtom(_) }

}
