object Combinator {
  def B1: (CFunc, CFunc) => Option[CFunc] = (a: CFunc, b: CFunc) => {
    if (a.rhs == b.lhs && a.forward && b.forward) {
      Some(CFunc(a.lhs, b.rhs, forward = true))
    } else {
      None
    }
  }

  def B2: (CFunc, CFunc) => Option[CFunc] = (b: CFunc, a: CFunc) => {
    if (a.rhs == b.lhs && !a.forward && b.forward) {
      Some(CFunc(a.lhs, b.rhs, forward = true))
    } else {
      None
    }
  }

  def S1: (CFunc, CFunc) => Option[CFunc] = (a: CFunc, b: CFunc) => {
    a.lhs match {
      case CFunc(lhslhs, lhsrhs, lhsforward) =>
        if (lhsrhs == b.lhs && a.rhs == b.rhs && lhsforward && a.forward && b.forward) {
          Some(CFunc(lhslhs, a.rhs, forward = true))
        } else {
          None
        }
      case _ => None
    }
  }

  def S2: (CFunc, CFunc) => Option[CFunc] = (b: CFunc, a: CFunc) => {
    a.lhs match {
      case CFunc(lhslhs, lhsrhs, lhsforward) =>
        if (lhsrhs == b.lhs && a.rhs == b.rhs && !lhsforward && a.forward && b.forward) {
          Some(CFunc(lhslhs, a.rhs, forward = true))
        } else {
          None
        }
      case _ => None
    }
  }

  def W1: (CFunc, CFunc) => Option[CFunc] = (a: CFunc, b: CFunc) => {
    a.lhs match {
      case CFunc(lhslhs: CFunc, lhsrhs, lhsforward) => {
        if (lhsrhs == b && lhsforward && a.forward) {
          Some(lhslhs)
        } else {
          None
        }
      }
      case _ => None
    }
  }

  def W2: (CFunc, CFunc) => Option[CFunc] = (b: CFunc, a: CFunc) => {
    a.lhs match {
      case CFunc(lhslhs: CFunc, lhsrhs, lhsforward) => {
        if (lhsrhs == b && !lhsforward && a.forward) {
          Some(lhslhs)
        } else {
          None
        }
      }
      case _ => None
    }
  }
}