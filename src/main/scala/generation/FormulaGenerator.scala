package ttqg.generation

import ttqg.logic._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.concurrent.Future

object FormulaGenerator {

  def generateGenericFormula(
      atoms: List[Atom],
      atomOnlyUnary: Boolean
  ): GenericFormula = {
    val r = new scala.util.Random
    var genericListBuffer =
      ListBuffer[GenericFormula](
        atoms
          .map(a =>
            if (r.nextBoolean()) AtomNode(a)
            else UnaryNode(AtomNode(a))
          )
          .toSeq: _*
      )
    while (genericListBuffer.size > 1) {
      val indexFrom = r.between(0, genericListBuffer.size)
      val chainFrom = genericListBuffer(indexFrom)
      genericListBuffer.remove(indexFrom)
      val indexTo = r.between(0, genericListBuffer.size)
      val chainTo = genericListBuffer(indexTo)
      genericListBuffer.remove(indexTo)
      val newGeneric =
        if (atomOnlyUnary || r.nextBoolean()) BinaryNode(chainFrom, chainTo)
        else UnaryNode(BinaryNode(chainFrom, chainTo))
      genericListBuffer.addOne(newGeneric)
    }
    return genericListBuffer.head
  }

  def generateFormula(
      atoms: Set[Atom],
      duplicateMax: Int,
      useAllAtoms: Boolean,
      atomOnlyUnary: Boolean,
      unaryOperators: Set[UnaryOperator.Value] = UnaryOperator.values,
      binaryOperators: Set[BinaryOperator.Value] = BinaryOperator.values
  ): Future[Formula] = {
    val atomListBuffer =
      if (useAllAtoms)
        ListBuffer[Atom](atoms.toSeq: _*)
      else
        Random
          .shuffle(ListBuffer[Atom](atoms.toSeq: _*))
          .take(Random.between(1, atoms.size + 1))
    val listBufferClone = atomListBuffer.clone()
    (0 to duplicateMax).foreach(_ =>
      listBufferClone.foreach(a =>
        if (Random.nextBoolean()) atomListBuffer += a
      )
    )
    return Future(
      generateGenericFormula(atomListBuffer.toList, atomOnlyUnary)
        .generateFormula(unaryOperators, binaryOperators)
    )
  }

  /*
  def generateEntailed(
      formula: Formula,
      useAllAtoms: Boolean,
      duplicateAtoms: Boolean,
      atomOnlyUnary: Boolean,
      unaryOperators: Set[UnaryOperator.Value] = UnaryOperator.values,
      binaryOperators: Set[BinaryOperator.Value] = BinaryOperator.values
  ): Formula = {
    val r = Random
    val formulaAtomSet = formula.getAtoms()
    var atomListBuffer = ListBuffer[Atom](formulaAtomSet.toSeq: _*)
    if (!useAllAtoms) {
      Random.shuffle(atomListBuffer)
      atomListBuffer =
        atomListBuffer.take(Random.between(1, atomListBuffer.size + 1))
    }
    if (duplicateAtoms) {
      for (a <- atomListBuffer)
        if (r.nextBoolean())
          atomListBuffer += a
    }
    val formula2: Formula =
      generateGenericFormula(atomListBuffer.toList, atomOnlyUnary)
        .generateFormula(unaryOperators, binaryOperators)
    if (formula.entails(formula2))
      return formula2
    return generateEntailed(
      formula,
      useAllAtoms,
      duplicateAtoms,
      atomOnlyUnary,
      unaryOperators,
      binaryOperators
    )
  }

  def generateConsistent(
      formula: Formula,
      duplicateMax: Int,
      useAllAtoms: Boolean,
      atomOnlyUnary: Boolean,
      unaryOperators: Set[UnaryOperator.Value] = UnaryOperator.values,
      binaryOperators: Set[BinaryOperator.Value] = BinaryOperator.values
  ): Formula = {
    val r = Random
    val formulaAtomSet = formula.getAtoms()
    var atomListBuffer = ListBuffer[Atom](formulaAtomSet.toSeq: _*)
    if (!useAllAtoms) {
      Random.shuffle(atomListBuffer)
      atomListBuffer =
        atomListBuffer.take(Random.between(1, atomListBuffer.size + 1))
    }
    for (i <- 0 to duplicateMax) {
      for (a <- atomListBuffer)
        if (r.nextBoolean())
          atomListBuffer += a
    }
    val formula2: Formula =
      generateGenericFormula(atomListBuffer.toList, atomOnlyUnary)
        .generateFormula(unaryOperators, binaryOperators)
    if (PropertyChecking.isConsistent(new KnowledgeBase(formula, formula2)))
      return formula2
    return generateConsistent(
      formula,
      useAllAtoms,
      duplicateMax,
      atomOnlyUnary,
      unaryOperators,
      binaryOperators
    )
  }

  def generateConclusion(
      premise: Formula,
      useAllAtoms: Boolean,
      duplicateAtoms: Boolean,
      atomOnlyUnary: Boolean,
      unaryOperators: Set[UnaryOperator.Value] = UnaryOperator.values,
      binaryOperators: Set[BinaryOperator.Value] = BinaryOperator.values
  ): Formula = {
    val r = Random
    val formulaAtomSet = premise.getAtoms()
    var atomListBuffer = ListBuffer[Atom](formulaAtomSet.toSeq: _*)
    if (!useAllAtoms) {
      Random.shuffle(atomListBuffer)
      atomListBuffer =
        atomListBuffer.take(Random.between(1, atomListBuffer.size + 1))
    }
    if (duplicateAtoms) {
      for (a <- atomListBuffer)
        if (r.nextBoolean())
          atomListBuffer += a
    }
    val conclusion: Formula =
      generateGenericFormula(atomListBuffer.toList, atomOnlyUnary)
        .generateFormula(unaryOperators, binaryOperators)
    if (PropertyChecking.isValid(new KnowledgeBase(premise), conclusion))
      return conclusion
    return generateConclusion(
      premise,
      useAllAtoms,
      duplicateAtoms,
      atomOnlyUnary,
      unaryOperators,
      binaryOperators
    )
  }

  def generateEquivalent(
      formula: Formula,
      useAllAtoms: Boolean,
      duplicateAtoms: Boolean,
      atomOnlyUnary: Boolean
  ): Formula = {
    val r = Random
    val formulaAtomSet = formula.getAtoms()
    var atomListBuffer = ListBuffer[Atom](formulaAtomSet.toSeq: _*)
    if (!useAllAtoms) {
      Random.shuffle(atomListBuffer)
      atomListBuffer =
        atomListBuffer.take(Random.between(1, atomListBuffer.size + 1))
    }
    if (duplicateAtoms) {
      for (a <- atomListBuffer)
        if (r.nextBoolean())
          atomListBuffer += a
    }
    val formula2: Formula =
      generateGenericFormula(atomListBuffer.toList, atomOnlyUnary)
        .generateFormula()
    if (PropertyChecking.isTruthFunctionallyEquivalent(formula, formula2))
      return formula2
    return generateEquivalent(
      formula,
      useAllAtoms,
      duplicateAtoms,
      atomOnlyUnary
    )
  }

  def generateTruthFunctional(
      truthFunctionalness: TruthFunctional.Value,
      atoms: Set[Atom],
      useAllAtoms: Boolean,
      duplicateAtoms: Boolean,
      atomOnlyUnary: Boolean
  ): Formula = {
    val r = Random
    var atomListBuffer = ListBuffer[Atom](atoms.toSeq: _*)
    if (!useAllAtoms) {
      atomListBuffer = Random.shuffle(atomListBuffer)
      atomListBuffer =
        atomListBuffer.take(Random.between(1, atomListBuffer.size + 1))
    }
    if (duplicateAtoms) {
      for (a <- atomListBuffer)
        if (r.nextBoolean())
          atomListBuffer += a
    }
    val formula: Formula =
      generateGenericFormula(atomListBuffer.toList, atomOnlyUnary)
        .generateFormula()
    if (
      PropertyChecking.getTruthFunctionalness(formula) == TruthFunctional.True
    )
      return formula
    return generateTruthFunctional(
      truthFunctionalness,
      atoms,
      useAllAtoms,
      duplicateAtoms,
      atomOnlyUnary
    )
  }

   */
}
