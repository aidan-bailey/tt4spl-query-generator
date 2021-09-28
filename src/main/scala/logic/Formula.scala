package ttqg.logic

import org.tweetyproject.logics.pl.syntax.PlFormula
import org.tweetyproject.logics.pl.syntax.Proposition
import org.tweetyproject.logics.pl.sat.Sat4jSolver
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import org.tweetyproject.logics.pl.sat.SatSolver
import org.tweetyproject.logics.pl.syntax.PlBeliefSet
import scala.jdk.CollectionConverters._
import org.tweetyproject.logics.pl.semantics.PossibleWorld
import collection.mutable

sealed abstract trait Formula {
  SatSolver.setDefaultSolver(new Sat4jSolver())
  private val solver = new Sat4jSolver()
  private val reasoner = new SatReasoner()

  def asPlFormula: PlFormula

  def evaluate(valuation: Valuation): Boolean =
    new PossibleWorld(
      valuation
        .filterKeys(a => valuation(a))
        .keySet
        .intersect(getAtoms())
        .map(a => new Proposition(a.toString()))
        .asJava
    ).satisfies(asPlFormula)

  def getAtoms(): Set[Atom]

  def getWorld(): Set[Valuation] = {
    var atomSeq = getAtoms().toSeq
    var atomCount = atomSeq.size
    var valuations = mutable.ListBuffer[Valuation]()
    var bound = math.pow(2, atomCount).toInt
    for (i <- 0 to bound - 1) {
      var valuation = mutable.Map[Atom, Boolean]()
      var binString = i.toBinaryString
      binString = "0" * (atomCount - binString.length()) + binString
      for (j <- 0 to atomCount - 1) {
        var assignment = binString(j) == '1'
        var atom = atomSeq(j)
        valuation(Atom(atom.toString())) = assignment
      }
      valuations += new Valuation(valuation.toMap)
    }
    return valuations.toSet
  }

  def getModels(): Set[Valuation] = getWorld().filter(evaluate(_))

  def entails(formula: Formula) =
    reasoner.query(asPlFormula, formula.asPlFormula)

  def isSatisfiable() = solver.isSatisfiable(new PlBeliefSet() {
    asPlFormula
  })

  def isConsistent() = solver.isConsistent(asPlFormula)

  def isTautology() = getWorld().size == getModels().size

  def isContradiction() = !isSatisfiable()

}

case class Atom(name: String) extends Formula {
  override def getAtoms(): Set[Atom] = Set(this)
  override def asPlFormula: PlFormula = new Proposition(name)
  override def toString(): String = name
}

sealed class BinaryFormula(
    operator: BinaryOperator.Value,
    operand1: Formula,
    operand2: Formula
) extends Formula {

  override def getAtoms(): Set[Atom] =
    operand1.getAtoms() ++ operand2.getAtoms()

  override def asPlFormula: PlFormula =
    BinaryOperator.toPlFormula(operator, operand1, operand2)

  override def toString(): String =
    s"(${operand1}${BinaryOperator.notationMap(operator)}${operand2})"
}

case class And(operand1: Formula, operand2: Formula)
    extends BinaryFormula(BinaryOperator.Conjunction, operand1, operand2)

case class Or(operand1: Formula, operand2: Formula)
    extends BinaryFormula(BinaryOperator.Disjunction, operand1, operand2)

case class Implies(operand1: Formula, operand2: Formula)
    extends BinaryFormula(BinaryOperator.Implication, operand1, operand2)

case class Iff(operand1: Formula, operand2: Formula)
    extends BinaryFormula(BinaryOperator.Equivalence, operand1, operand2)

sealed class UnaryFormula(operator: UnaryOperator.Value, operand: Formula)
    extends Formula {

  override def getAtoms(): Set[Atom] = operand.getAtoms()

  override def asPlFormula: PlFormula =
    UnaryOperator.toPlFormula(operator, operand)

  override def toString(): String =
    s"${UnaryOperator.notationMap(operator)}$operand"

}

case class Not(operand: Formula)
    extends UnaryFormula(UnaryOperator.Negation, operand)
