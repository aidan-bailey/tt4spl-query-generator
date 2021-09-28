package ttqg.logic

import org.tweetyproject.logics.pl.syntax

sealed trait Operator extends Enumeration

object BinaryOperator extends Operator {
  val Conjunction, Disjunction, Implication, Equivalence = Value
  def toPlFormula(
      operator: BinaryOperator.Value,
      operand1: Formula,
      operand2: Formula
  ): syntax.PlFormula =
    operator match {
      case Conjunction =>
        new syntax.Conjunction(operand1.asPlFormula, operand2.asPlFormula)
      case Disjunction =>
        new syntax.Disjunction(operand1.asPlFormula, operand2.asPlFormula)
      case Implication =>
        new syntax.Implication(operand1.asPlFormula, operand2.asPlFormula)
      case Equivalence =>
        new syntax.Equivalence(operand1.asPlFormula, operand2.asPlFormula)
    }
  val notationMap = Map[BinaryOperator.Value, String](
    Conjunction -> "&&",
    Disjunction -> "||",
    Implication -> "=>",
    Equivalence -> "<=>"
  )

}

object UnaryOperator extends Operator {
  val Negation = Value
  def toPlFormula(
      operator: UnaryOperator.Value,
      operand: Formula
  ): syntax.PlFormula =
    operator match {
      case Negation =>
        new syntax.Negation(operand.asPlFormula)
    }
  val notationMap = Map[UnaryOperator.Value, String](
    Negation -> "!"
  )

}
