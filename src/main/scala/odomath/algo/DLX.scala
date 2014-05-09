package odomath.algo

import collection.mutable
import scala.util.Try
import odoutil._

/**
 * User: Oleg
 * Date: 03.02.13
 * Time: 13:15
 */
class DLX[P, C](initial: Seq[P], all: Seq[P], constraints: P => Seq[C]) {

  trait Removable {
    def remove

    def restore
  }

  sealed trait DequeItemLeft {
    var next: DequeItemRight
  }

  sealed trait DequeItemRight {
    var prev: DequeItemLeft
  }

  case class DequeItem[T](value: T, override var prev: DequeItemLeft) extends DequeItemLeft with DequeItemRight with Removable {
    override var next = prev.next
    prev.next = this

    def remove {
      prev.next = next
      next.prev = prev
    }

    def restore {
      prev.next = this
      next.prev = this
    }

    override def toString = "di/" + value.toString
  }

  class Deque[T] extends Start {
    var last: DequeItemLeft = this

    def add(value: T) = {
      val item = new DequeItem(value, last)
      last = item
      item
    }

    def toList = {
      def iter(acc: List[DequeItem[T]], item: DequeItemRight): List[DequeItem[T]] = item match {
        case End(_) => acc
        case item: DequeItem[T] => iter(item :: acc, item.next)
      }
      iter(Nil, this.next)
    }

    def isEmpty = next match {
      case End(_) => true
      case _ => false
    }

    override def toString = toList match {
      case list => list.length + " " + list.toString
    }
  }

  implicit class RemovableList[Q <: Removable](seq: Seq[Q]) {
    def removeAll = seq foreach (_.remove)

    def restoreAll = seq.reverse foreach (_.restore)
  }

  case class End(var prev: DequeItemLeft) extends DequeItemRight

  class Start extends DequeItemLeft {
    override var next: DequeItemRight = End(this)
  }

  case class Possibility(value: P) {
    val variants = new Deque[Variant]

    def choose = {
      val thisVariants = variants.toList
      val cons2remove = thisVariants.map(_.value.constraint)
      val poss2remove = cons2remove.flatMap(_.value.variants.toList.map(_.value.possibility)).toSet.toList
      val vars2remove = poss2remove.flatMap(_.value.variants.toList).map(_.value)
      List.empty[Removable] ++ cons2remove ++ poss2remove ++ vars2remove
    }
  }

  object Possibility {

    var possibilities = new Deque[Possibility]
    val byValue = mutable.Map.empty[P, DequeItem[Possibility]].updateDefault(value => possibilities.add(Possibility(value)))

    def initialize {
      for (value <- all) {
        val possibility = byValue(value)
        for (constraint: DequeItem[Constraint] <- constraints(possibility.value.value).map(Constraint.byValue))
          Variant(constraint, possibility)
      }
    }
  }

  case class Constraint(value: C) {
    private var myVariants = 0
    val variants = new Deque[Variant]

    def variantCount = myVariants

    def variantCount_=(variants: Int) {
      myVariants = variants
    }
  }

  object Constraint {
    var constraints = new Deque[Constraint]

    val byValue = mutable.Map.empty[C, DequeItem[Constraint]].updateDefault(value => constraints.add(Constraint(value)))
  }

  case class Variant(constraint: DequeItem[Constraint], possibility: DequeItem[Possibility]) extends Removable {
    val constraintItem = constraint.value.variants.add(this)
    val possibilityItem = possibility.value.variants.add(this)
    constraint.value.variantCount += 1

    def remove {
      constraintItem.remove
      possibilityItem.remove
      constraint.value.variantCount -= 1
    }

    def restore {
      constraintItem.restore
      possibilityItem.restore
      constraint.value.variantCount += 1
    }
  }

  def initialize {
    Possibility.initialize
    initial.map(Possibility.byValue).foreach(_.value.choose.removeAll)
  }

  lazy val solution = {
    initialize
    def solve(solution: Seq[P]): Option[Seq[P]] = if (Constraint.constraints.isEmpty) Some(solution)
    else {
      val constraint = Constraint.constraints.toList.map(_.value).minBy(_.variantCount)
      if (constraint.variantCount == 0) None
      else constraint.variants.toList.map(_.value.possibility.value).toStream.map(possibility => {
        val removeList = possibility.choose
        removeList.removeAll
        val lower = solve(possibility.value +: solution)
        removeList.restoreAll
        lower
      }).find(_ != None).flatten
    }
    Try(solve(initial)).toOption.flatten
  }
}
