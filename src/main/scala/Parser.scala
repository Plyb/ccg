import scala.collection.mutable.Queue

object Parser {
  def parse(sentence: List[Constituent]): Set[Constituent] = {
    val q: Queue[List[Constituent]] = Queue()
    val combinators = List(Combinator.B1, Combinator.B2, Combinator.S1, Combinator.S2, Combinator.W1, Combinator.W2)
    q.enqueue(sentence)

    def replace(c: Constituent)(implicit v: CVar, r: Constituent): Constituent = {
      c match {
        case CFunc(lhs, rhs, forward) => CFunc(replace(lhs)(v, r), replace(rhs)(v, r), forward)
        case Primitive(name) => Primitive(name)
        case CVar(n) => {
          if (n == v.n) r else CVar(n)
        }
      }
    }

    def getMapping(a: Constituent, b: Constituent) : Map[CVar, Constituent] = {
      if (a.isInstanceOf[CVar]) {
        Map((a.asInstanceOf[CVar], b))
      } else if (b.isInstanceOf[CVar]) {
        Map((b.asInstanceOf[CVar], a))
      } else {
        a match {
          case CFunc(lhs, rhs, _) if b.isInstanceOf[CFunc] => {
            val bFunc = b.asInstanceOf[CFunc]
            getMapping(lhs, bFunc.lhs) ++ getMapping(rhs, bFunc.rhs)
          }
          case _ => Map()
        }
      }
    }

    def applyConstituent(a: Constituent, b: Constituent): Option[Constituent] = {
      a match {
        case CFunc(lhs, rhs, true) if rhs == b => {
          val mapping = getMapping(rhs, b)
          val mapped = mapping.foldRight(lhs)((currentMapping, c) => {
            replace(c)(currentMapping._1, currentMapping._2)
          })
          Some(mapped)
        }
        case _ =>
          b match {
            case CFunc(lhs, rhs, false) if rhs == a => Some(lhs)
            case _ => None
          }
      }
    }

    var results = Set[Constituent]()
    while (q.nonEmpty) {
      val sentence = q.dequeue
      if (sentence.length == 1) {
        results += sentence.head
      } else {
        val newSentences = sentence.zipWithIndex.flatMap({
          case (cfunc: CFunc, i) if (i < sentence.length - 1 && sentence(i + 1).isInstanceOf[CFunc]) => {
            val modifiedConstituents = combinators.flatMap(combinator => {
              combinator(cfunc, sentence(i + 1).asInstanceOf[CFunc])
            })
            modifiedConstituents.map(sentence.slice(0, i).appended(_).appendedAll(sentence.slice(i + 2, sentence.length)))
          }
          case _ => List.empty
        })
        newSentences.foreach(q.enqueue)

        val appliedSentences = sentence.zipWithIndex.flatMap(x => {
          if (x._2 == sentence.length - 1) {
            None
          } else {
            applyConstituent(x._1, sentence(x._2 + 1)) match {
              case Some(appliedConstituent) => Some(
                sentence.slice(0, x._2).appended(appliedConstituent).appendedAll(sentence.slice(x._2 + 2, sentence.length))
              )
              case None => None
            }
          }
        })
        appliedSentences.foreach(q.enqueue)
      }
    }
    results
  }
}
