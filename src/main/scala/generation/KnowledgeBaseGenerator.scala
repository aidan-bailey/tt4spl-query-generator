package ttqg.generation

import ttqg.logic._
import scala.util.Random
import scala.collection.mutable.ListBuffer

object KnowledgeBaseGenerator {

  def generateConsistent(
      atoms: Set[Atom],
      size: Int,
      useAllAtoms: Boolean,
      duplicateAtoms: Boolean,
      atomOnlyUnary: Boolean,
      unaryOperators: Set[UnaryOperator.Value],
      binaryOperators: Set[BinaryOperator.Value],
      maxRecDepth: Int = 10000
  ): Option[KnowledgeBase] = {
    if (maxRecDepth < 1)
      return None
    val kb = new KnowledgeBase(
      List.fill(size)(
        FormulaGenerator.generateFormula(
          atoms,
          useAllAtoms,
          duplicateAtoms,
          atomOnlyUnary,
          unaryOperators,
          binaryOperators
        )
      )
    )
    if (
      PropertyChecking
        .getTruthFunctionalness(kb)
        .equals(TruthFunctional.Indeterminate) && kb.isConsistent()
    )
      return Some(kb)
    return generateConsistent(
      atoms,
      size,
      useAllAtoms,
      duplicateAtoms,
      atomOnlyUnary,
      unaryOperators,
      binaryOperators,
      maxRecDepth - 1
    )
  }

  def generateTruthFunctional(
      property: TruthFunctional.Value,
      atoms: Set[Atom],
      size: Int,
      useAllAtoms: Boolean,
      duplicateAtoms: Boolean,
      atomOnlyUnary: Boolean,
      unaryOperators: Set[UnaryOperator.Value],
      binaryOperators: Set[BinaryOperator.Value],
      maxRecDepth: Int = 10000
  ): Option[KnowledgeBase] = {
    if (maxRecDepth < 1)
      return None
    val kb = new KnowledgeBase(
      List.fill(size)(
        FormulaGenerator.generateFormula(
          atoms,
          useAllAtoms,
          duplicateAtoms,
          atomOnlyUnary,
          unaryOperators,
          binaryOperators
        )
      )
    )

    if (PropertyChecking.getTruthFunctionalness(kb).equals(property))
      return Some(kb)
    return generateTruthFunctional(
      property,
      atoms,
      size,
      useAllAtoms,
      duplicateAtoms,
      atomOnlyUnary,
      unaryOperators,
      binaryOperators,
      maxRecDepth - 1
    )
  }

  def generateEntailment(
      atoms: Set[Atom],
      premiseSize: Int,
      useAllAtoms: Boolean,
      duplicateAtoms: Boolean,
      atomOnlyUnary: Boolean,
      unaryOperators: Set[UnaryOperator.Value],
      binaryOperators: Set[BinaryOperator.Value],
      maxRecDepth: Int = 10000
  ): Option[(KnowledgeBase, Formula)] = {
    if (maxRecDepth < 1)
      return None
    val kb = new KnowledgeBase(
      List.fill(premiseSize)(
        FormulaGenerator.generateFormula(
          atoms,
          useAllAtoms,
          duplicateAtoms,
          atomOnlyUnary,
          unaryOperators,
          binaryOperators
        )
      )
    )
    val conclusion = FormulaGenerator.generateFormula(
      atoms,
      useAllAtoms,
      duplicateAtoms,
      atomOnlyUnary,
      unaryOperators,
      binaryOperators
    )
    if (
      PropertyChecking
        .getTruthFunctionalness(kb)
        .equals(TruthFunctional.Indeterminate) && kb.entails(conclusion)
    )
      return Some(kb, conclusion)
    return generateEntailment(
      atoms,
      premiseSize,
      useAllAtoms,
      duplicateAtoms,
      atomOnlyUnary,
      unaryOperators,
      binaryOperators,
      maxRecDepth - 1
    )
  }

  def generateArgument(
      atoms: Set[Atom],
      premiseSize: Int,
      useAllAtoms: Boolean,
      duplicateAtoms: Boolean,
      atomOnlyUnary: Boolean,
      unaryOperators: Set[UnaryOperator.Value],
      binaryOperators: Set[BinaryOperator.Value],
      maxRecDepth: Int = 10000
  ): Option[(KnowledgeBase, Formula)] = {
    if (maxRecDepth < 1)
      return None
    val kb = new KnowledgeBase(
      List.fill(premiseSize)(
        FormulaGenerator.generateFormula(
          atoms,
          useAllAtoms,
          duplicateAtoms,
          atomOnlyUnary,
          unaryOperators,
          binaryOperators
        )
      )
    )
    val conclusion = FormulaGenerator.generateFormula(
      atoms,
      useAllAtoms,
      duplicateAtoms,
      atomOnlyUnary,
      unaryOperators,
      binaryOperators
    )
    if (
      PropertyChecking
        .getTruthFunctionalness(kb)
        .equals(TruthFunctional.Indeterminate) &&
      PropertyChecking
        .getTruthFunctionalness(conclusion)
        .equals(TruthFunctional.Indeterminate) &&
      PropertyChecking.isValid(
        kb,
        conclusion
      )
    )
      return Some(kb, conclusion)
    return generateArgument(
      atoms,
      premiseSize,
      useAllAtoms,
      duplicateAtoms,
      atomOnlyUnary,
      unaryOperators,
      binaryOperators,
      maxRecDepth - 1
    )
  }

  def generateEquivalent(
      atoms: Set[Atom],
      size: Int,
      duplicateAtoms: Boolean,
      atomOnlyUnary: Boolean,
      unaryOperators: Set[UnaryOperator.Value],
      binaryOperators: Set[BinaryOperator.Value],
      maxRecDepth: Int = 10000
  ): Option[KnowledgeBase] = {
    if (size == 0)
      return Some(new KnowledgeBase())
    if (maxRecDepth < 1)
      return None
    val kb = new KnowledgeBase(
      List.fill(size)(
        FormulaGenerator.generateFormula(
          atoms,
          true,
          duplicateAtoms,
          atomOnlyUnary,
          unaryOperators,
          binaryOperators
        )
      )
    )
    val front = kb.iterator.next()
    if (
      PropertyChecking
        .getTruthFunctionalness(kb)
        .equals(TruthFunctional.Indeterminate) &&
      kb.iterator
        .forall(p => PropertyChecking.isTruthFunctionallyEquivalent(front, p))
    )
      return Some(kb)
    return generateEquivalent(
      atoms,
      size,
      duplicateAtoms,
      atomOnlyUnary,
      unaryOperators,
      binaryOperators,
      maxRecDepth - 1
    )
  }

}
