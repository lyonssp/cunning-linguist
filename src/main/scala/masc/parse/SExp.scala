package masc.parse

sealed trait SExp {
  def isAtom: Boolean
  def isLeaf: Boolean
}

case class SAtom(car: String) extends SExp {
  val isAtom = true
  val isLeaf = false
}

case class SList(car: SExp, cdr: Seq[SExp]) extends SExp {
  val isAtom = false
  val isLeaf = car.isAtom && (cdr.size == 1) && cdr.head.isAtom
}

