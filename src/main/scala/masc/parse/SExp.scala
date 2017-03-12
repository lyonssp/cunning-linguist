package masc.parse

sealed trait SExp {
  def isAtom: Boolean
  def isLeaf: Boolean
  def getLeaves: Seq[(String, String)]
}

case class SAtom(car: String) extends SExp {
  val isAtom = true
  val isLeaf = false
  def getLeaves = Seq()
}

case class SList(car: SExp, cdr: Seq[SExp]) extends SExp {
  val isAtom = false
  val isLeaf = car.isAtom && (cdr.size == 1) && cdr.head.isAtom
  def getLeaves = (car, cdr) match {
    case (SAtom(str1), Seq(SAtom(str2))) => Seq((str1, str2))
    case _ => car.getLeaves ++ cdr.flatMap(_.getLeaves)
  }
}
