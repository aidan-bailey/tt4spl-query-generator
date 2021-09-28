package ttqg.generation

import ttqg.logic._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue

class FormulaConstructor(
    genericFormula: GenericFormula,
    unaryOperators: Set[UnaryOperator.Value],
    binaryOperators: Set[BinaryOperator.Value]
) {

  def this(genericFormula: GenericFormula) =
    this(genericFormula, UnaryOperator.values, BinaryOperator.values)

  private var binIndex = 0
  private val binCount = genericFormula.getBinaryCount()
  private val unCount = genericFormula.getUnaryCount()
  private val binOpSeq = binaryOperators.toSeq
  private val limit = math.pow(binOpSeq.length, binCount).toInt

  private def dec2baseStr(num: Int, base: Int): String = {
    if (num >= (base - 1)) {
      return dec2baseStr(num / base, base) + (num % base).toString()
    } else
      return (num % base).toString()
  }

  def hasNext(): Boolean = binIndex < limit - 1

  def next(): Formula = {
    val sequ = binaryOperators.toSeq
    var baseStr = dec2baseStr(binIndex, binOpSeq.length)
    if (baseStr.length() < binCount)
      baseStr = "0" * (binCount - baseStr.length) + baseStr
    else if (baseStr.length() > binCount)
      baseStr = baseStr.drop(baseStr.length - binCount)
    var binQueue = new Queue[BinaryOperator.Value]()
    for (char <- baseStr.toCharArray()) {
      var op = binOpSeq(char.asDigit)
      binQueue.enqueue(op)
    }
    val unQueue = new Queue[UnaryOperator.Value]()
    for (i <- 0 to unCount - 1) {
      unQueue.enqueue(UnaryOperator.Negation)
    }
    binIndex += 1
    genericFormula.toFormula(unQueue, binQueue)
  }

  def getAll(): Set[Formula] = {
    var result: ListBuffer[Formula] = new ListBuffer()
    for (i <- 0 to limit - 1) {
      var baseStr = dec2baseStr(i, binOpSeq.length)
      if (baseStr.length() < binCount)
        baseStr = "0" * (binCount - baseStr.length) + baseStr
      else if (baseStr.length() > binCount)
        baseStr = baseStr.drop(baseStr.length - binCount)
      var binQueue = new Queue[BinaryOperator.Value]()
      for (char <- baseStr.toCharArray()) {
        var op = binOpSeq(char.asDigit)
        binQueue.enqueue(op)
      }
      val unQueue = new Queue[UnaryOperator.Value]()
      for (i <- 0 to unCount - 1) {
        unQueue.enqueue(UnaryOperator.Negation)
      }
      val formula: Formula = genericFormula.toFormula(unQueue, binQueue)
      result += formula
    }
    return result.toList.toSet
  }

}
