package ttqg.logic

import scala.collection.mutable
import org.tweetyproject.logics.pl.syntax.PlBeliefSet
import scala.jdk.CollectionConverters._
import org.tweetyproject.logics.pl.sat.SatSolver
import org.tweetyproject.logics.pl.sat.Sat4jSolver
import org.tweetyproject.logics.pl.reasoner.SatReasoner
import org.tweetyproject.logics.pl.syntax.Contradiction
import org.tweetyproject.logics.pl.syntax.Tautology
import org.tweetyproject.logics.pl.semantics.PossibleWorld
import org.tweetyproject.logics.pl.syntax.Proposition

class KnowledgeBase(k: mutable.Set[Formula]) {

  SatSolver.setDefaultSolver(new Sat4jSolver())
  private val solver = new Sat4jSolver()
  private val reasoner = new SatReasoner()

  def this(formula: Formula*) = this(mutable.Set(formula: _*))

  def this(formulas: IterableOnce[Formula]) = this(formulas.toSeq: _*)

  def this(knowledgeBase: KnowledgeBase) =
    this(knowledgeBase.iterator.toSeq: _*)

  def asPlBeliefSet: PlBeliefSet =
    new PlBeliefSet(
      k.map(f => f.asPlFormula).asJava
    )

  def toSet: Set[Formula] = Set(k.clone().toSeq: _*)

  def evaluate(valuation: Valuation): Boolean = {
    new PossibleWorld(
      valuation
        .filterKeys(a => valuation(a))
        .keySet
        .intersect(getAtoms())
        .map(a => new Proposition(a.toString()))
        .asJava
    ).satisfies(asPlBeliefSet)
  }

  def getAtoms(): Set[Atom] = {
    k.foldLeft(mutable.Set[Atom]())((atoms, formula) =>
      atoms.addAll(formula.getAtoms())
    ).toSet
  }

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

  def entails(formula: Formula): Boolean =
    reasoner.query(asPlBeliefSet, formula.asPlFormula)

  def isSatisfiable() = solver.isSatisfiable(
    asPlBeliefSet
  )

  def isConsistent() = solver.isConsistent(asPlBeliefSet)

  def isContradiction() = !isSatisfiable()

  def isTautology() = getModels().size.equals(getWorld().size)

  def add(formula: Formula): this.type = {
    k.add(formula)
    this
  }

  def +(formula: Formula): KnowledgeBase = clone().add(formula)

  def +=(formula: Formula): this.type = add(formula)

  def addAll(formulas: IterableOnce[Formula]): this.type = {
    k.addAll(formulas)
    this
  }

  def ++(knowledgeBase: KnowledgeBase): KnowledgeBase =
    clone().addAll(knowledgeBase.iterator)

  def ++=(knowledgeBase: KnowledgeBase): this.type =
    addAll(knowledgeBase.iterator)

  def remove(formula: Formula): this.type = {
    k.remove(formula)
    this
  }

  def -(formula: Formula): KnowledgeBase = clone().remove(formula)

  def -=(formula: Formula): this.type = remove(formula)

  def removeAll(formulas: IterableOnce[Formula]): this.type = {
    formulas.foreach(k.remove(_))
    this
  }

  def --(knowledgeBase: KnowledgeBase): KnowledgeBase =
    clone().removeAll(knowledgeBase.iterator)

  def --=(knowledgeBase: KnowledgeBase): this.type =
    removeAll(knowledgeBase.iterator)

  def union(knowledgeBase: KnowledgeBase): KnowledgeBase =
    clone().addAll(knowledgeBase.iterator)

  def intersection(knowledgeBase: KnowledgeBase): KnowledgeBase =
    new KnowledgeBase(iterator.filter(knowledgeBase.contains(_)))

  def iterator: Iterator[Formula] = k.iterator

  def size = iterator.size

  def contains(formula: Formula): Boolean = k.contains(formula)

  def clear(): Unit = k.clear()

  override def clone(): KnowledgeBase = new KnowledgeBase(k.clone())

  override def toString(): String = s"{${iterator.mkString(", ")}}"

}
