sealed trait Constituent {
  def /(other: Constituent) = CFunc(this, other, forward = true)
  def \(other: Constituent) = CFunc(this, other, forward = false)
}
case class Primitive(name: String) extends Constituent {
  override def toString: String = name
}
case class CFunc(lhs: Constituent, rhs: Constituent, forward: Boolean) extends Constituent {
  override def toString: String = "(" + lhs.toString + (if (forward) "/" else "\\") + rhs.toString + ")"
}
case class CVar(n: Int) extends Constituent {
  override def toString: String = n.toString

  override def equals(obj: Any): Boolean = true
}

object Constituent {
  private var n = 0;
  def freshCVar(): CVar = {
    n += 1
    CVar(n)
  }
}