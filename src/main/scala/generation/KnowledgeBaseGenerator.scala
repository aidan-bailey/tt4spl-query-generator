package ttqg.generation

import ttqg.logic._
import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.util.Success
import scala.concurrent.Await
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import java.util.concurrent.ExecutorService
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.ForkJoinPool

object KnowledgeBaseGenerator {

  def generateConsistent(
      antiTest: Boolean,
      atoms: Set[Atom],
      size: Int,
      duplicateMax: Int,
      useAllAtoms: Boolean,
      atomOnlyUnary: Boolean,
      unaryOperators: Set[UnaryOperator.Value],
      binaryOperators: Set[BinaryOperator.Value],
      maxRecDepth: Int = 10000
  ): Future[Option[KnowledgeBase]] = {
    if (maxRecDepth < 1)
      return Future(None)
    val kb = new KnowledgeBase(
      List
        .fill(size)(
          FormulaGenerator
            .generateFormula(
              atoms,
              duplicateMax,
              useAllAtoms,
              atomOnlyUnary,
              unaryOperators,
              binaryOperators
            )
        )
        .map(Await.result(_, Duration(5, TimeUnit.MINUTES)))
    )
    if (!antiTest) {
      if (
        PropertyChecking
          .getTruthFunctionalness(kb)
          .equals(TruthFunctional.Indeterminate) && kb.isConsistent()
      )
        return Future(Some(kb))
    } else {
      if (
        PropertyChecking
          .getTruthFunctionalness(kb)
          .equals(TruthFunctional.Indeterminate) && !kb.isConsistent()
      )
        return Future(Some(kb))
    }
    return generateConsistent(
      antiTest,
      atoms,
      size,
      duplicateMax,
      useAllAtoms,
      atomOnlyUnary,
      unaryOperators,
      binaryOperators,
      maxRecDepth - 1
    )
  }

  def generateTruthFunctional(
      antiTest: Boolean,
      property: TruthFunctional.Value,
      atoms: Set[Atom],
      size: Int,
      duplicateMax: Int,
      useAllAtoms: Boolean,
      atomOnlyUnary: Boolean,
      unaryOperators: Set[UnaryOperator.Value],
      binaryOperators: Set[BinaryOperator.Value],
      maxRecDepth: Int = 10000
  ): Future[Option[KnowledgeBase]] = {
    if (maxRecDepth < 1)
      return Future(None)
    val kb = new KnowledgeBase(
      List
        .fill(size)(
          FormulaGenerator.generateFormula(
            atoms,
            duplicateMax,
            useAllAtoms,
            atomOnlyUnary,
            unaryOperators,
            binaryOperators
          )
        )
        .map(Await.result(_, Duration(5, TimeUnit.MINUTES)))
    )
    if (!antiTest) {
      if (PropertyChecking.getTruthFunctionalness(kb).equals(property))
        return Future(Some(kb))
    } else if (!PropertyChecking.getTruthFunctionalness(kb).equals(property))
      return Future(Some(kb))
    return generateTruthFunctional(
      antiTest,
      property,
      atoms,
      size,
      duplicateMax,
      useAllAtoms,
      atomOnlyUnary,
      unaryOperators,
      binaryOperators,
      maxRecDepth - 1
    )
  }

  def generateEntailment(
      antiTest: Boolean,
      atoms: Set[Atom],
      premiseSize: Int,
      duplicateMax: Int,
      useAllAtoms: Boolean,
      atomOnlyUnary: Boolean,
      unaryOperators: Set[UnaryOperator.Value],
      binaryOperators: Set[BinaryOperator.Value],
      maxRecDepth: Int = 10000
  ): Future[Option[(KnowledgeBase, Formula)]] = {
    if (maxRecDepth < 1)
      return Future(None)
    val kb = new KnowledgeBase(
      List
        .fill(premiseSize)(
          FormulaGenerator.generateFormula(
            atoms,
            duplicateMax,
            useAllAtoms,
            atomOnlyUnary,
            unaryOperators,
            binaryOperators
          )
        )
        .map(Await.result(_, Duration(5, TimeUnit.MINUTES)))
    )
    val conclusion: Formula = Await.result(
      FormulaGenerator
        .generateFormula(
          atoms,
          duplicateMax,
          useAllAtoms,
          atomOnlyUnary,
          unaryOperators,
          binaryOperators
        ),
      Duration(5, TimeUnit.MINUTES)
    )
    if (!antiTest) {
      if (
        PropertyChecking
          .getTruthFunctionalness(kb)
          .equals(TruthFunctional.Indeterminate) && kb.entails(conclusion)
      )
        return Future(Some(kb, conclusion))
    } else {
      if (
        PropertyChecking
          .getTruthFunctionalness(kb)
          .equals(TruthFunctional.Indeterminate) && !kb.entails(conclusion)
      )
        return Future(Some(kb, conclusion))
    }
    return generateEntailment(
      antiTest,
      atoms,
      premiseSize,
      duplicateMax,
      useAllAtoms,
      atomOnlyUnary,
      unaryOperators,
      binaryOperators,
      maxRecDepth - 1
    )
  }

  def generateArgument(
      antiTest: Boolean,
      atoms: Set[Atom],
      premiseSize: Int,
      duplicateMax: Int,
      useAllAtoms: Boolean,
      atomOnlyUnary: Boolean,
      unaryOperators: Set[UnaryOperator.Value],
      binaryOperators: Set[BinaryOperator.Value],
      maxRecDepth: Int = 10000
  ): Future[Option[(KnowledgeBase, Formula)]] = {
    if (maxRecDepth < 1)
      return Future(None)
    val kb = new KnowledgeBase(
      List
        .fill(premiseSize)(
          FormulaGenerator.generateFormula(
            atoms,
            duplicateMax,
            useAllAtoms,
            atomOnlyUnary,
            unaryOperators,
            binaryOperators
          )
        )
        .map(Await.result(_, Duration(5, TimeUnit.MINUTES)))
    )
    val conclusion: Formula = Await.result(
      FormulaGenerator
        .generateFormula(
          atoms,
          duplicateMax,
          useAllAtoms,
          atomOnlyUnary,
          unaryOperators,
          binaryOperators
        ),
      Duration(5, TimeUnit.MINUTES)
    )
    if (!antiTest) {
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
        return Future(Some(kb, conclusion))
    } else {
      if (
        PropertyChecking
          .getTruthFunctionalness(kb)
          .equals(TruthFunctional.Indeterminate) &&
        PropertyChecking
          .getTruthFunctionalness(conclusion)
          .equals(TruthFunctional.Indeterminate) &&
        !PropertyChecking.isValid(
          kb,
          conclusion
        )
      )
        return Future(Some(kb, conclusion))
    }
    return generateArgument(
      antiTest,
      atoms,
      premiseSize,
      duplicateMax,
      useAllAtoms,
      atomOnlyUnary,
      unaryOperators,
      binaryOperators,
      maxRecDepth - 1
    )
  }

  def generateEquivalent(
      antiTest: Boolean,
      atoms: Set[Atom],
      size: Int,
      duplicateMax: Int,
      atomOnlyUnary: Boolean,
      unaryOperators: Set[UnaryOperator.Value],
      binaryOperators: Set[BinaryOperator.Value],
      maxRecDepth: Int = 10000
  ): Future[Option[KnowledgeBase]] = {
    if (size == 0)
      return Future(Some(new KnowledgeBase()))
    if (maxRecDepth < 1)
      return Future(None)
    val kb = new KnowledgeBase(
      List
        .fill(size)(
          FormulaGenerator.generateFormula(
            atoms,
            duplicateMax,
            true,
            atomOnlyUnary,
            unaryOperators,
            binaryOperators
          )
        )
        .map(Await.result(_, Duration(3, TimeUnit.MINUTES)))
    )
    val front = kb.iterator.next()
    if (!antiTest) {
      if (
        kb.iterator
          .forall(p => PropertyChecking.isTruthFunctionallyEquivalent(front, p))
      )
        return Future(Some(kb))
    } else {
      if (
        !kb.iterator
          .forall(p => PropertyChecking.isTruthFunctionallyEquivalent(front, p))
      )
        return Future(Some(kb))
    }
    return generateEquivalent(
      antiTest,
      atoms,
      size,
      duplicateMax,
      atomOnlyUnary,
      unaryOperators,
      binaryOperators,
      maxRecDepth - 1
    )
  }

}
