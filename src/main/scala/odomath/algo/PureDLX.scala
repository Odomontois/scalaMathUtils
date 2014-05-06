package odomath.algo

/**
 * User: admin
 * Date: 03.02.13
 * Time: 13:30
 */
class PureDLX[P, C](initial: Seq[P], all: Seq[P], constraints: P => Seq[C]) {

  case class Possibility(value: P)

  object Possibility {
    val byValue = all.map(p => (p, Possibility(p))).toMap
  }

  case class Constraint(value: C)

  object Constraint {
    val byValue = all.flatMap(constraints).toSet[C].map(c => (c, Constraint(c))).toMap
  }

  case class Variant(possibility: Possibility, constraint: Constraint)

}
