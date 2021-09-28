package ttqg.generation

import ttqg.logic._
import scala.collection.mutable.Queue
import scala.util.Random

sealed abstract trait GenericFormula {
  def toFormula(
      unaryQueue: Queue[UnaryOperator.Value],
      binaryQueue: Queue[BinaryOperator.Value]
  ): Formula

  def getBinaryCount(): Int

  def getUnaryCount(): Int

  def getAtomCount(): Int

  def generateFormula(
      unaryOperators: Set[UnaryOperator.Value] = UnaryOperator.values,
      binaryOperators: Set[BinaryOperator.Value] = BinaryOperator.values
  ): Formula = {
    def dec2baseStr(num: Int, base: Int): String = {
      if (num >= (base - 1)) {
        return if (base > 1)
          dec2baseStr(num / base, base) + (num % base).toString()
        else
          dec2baseStr(num - 1, base) + (num % base).toString()
      } else
        return (num % base).toString()
    }
    val unCount = getUnaryCount()
    val unOpSeq = unaryOperators.toSeq
    val unLimit = math.pow(unOpSeq.length, unCount).toInt
    var unOpBinStr = dec2baseStr(Random.between(0, unLimit), unOpSeq.length)
    if (unOpBinStr.length() < unCount)
      unOpBinStr = "0" * (unCount - unOpBinStr.length) + unOpBinStr
    else if (unOpBinStr.length() > unCount)
      unOpBinStr = unOpBinStr.drop(unOpBinStr.length - unCount)

    val unQueue =
      unOpBinStr
        .foldLeft(Queue[UnaryOperator.Value]())((q, c) =>
          q.enqueue(unOpSeq(c.asDigit))
        )

    val binCount = getBinaryCount()
    val binOpSeq = binaryOperators.toSeq
    val binLimit = math.pow(binOpSeq.length, binCount).toInt
    var binOpBinStr =
      dec2baseStr(Random.between(0, binLimit), binOpSeq.length)
    if (binOpBinStr.length() < binCount)
      binOpBinStr = "0" * (binCount - binOpBinStr.length) + binOpBinStr
    else if (binOpBinStr.length() > binCount)
      binOpBinStr = binOpBinStr.drop(binOpBinStr.length - binCount)

    val binQueue =
      binOpBinStr
        .foldLeft(Queue[BinaryOperator.Value]())((q, c) =>
          q.enqueue(binOpSeq(c.asDigit))
        )

    toFormula(unQueue, binQueue)
  }

}

case class BinaryNode(leftChild: GenericFormula, rightChild: GenericFormula)
    extends GenericFormula {

  override def getBinaryCount(): Int =
    1 + leftChild.getBinaryCount() + rightChild.getBinaryCount()

  override def getUnaryCount(): Int =
    leftChild.getUnaryCount() + rightChild.getUnaryCount()

  override def getAtomCount(): Int =
    leftChild.getAtomCount() + rightChild.getAtomCount()

  override def toFormula(
      unaryQueue: Queue[UnaryOperator.Value],
      binaryQueue: Queue[BinaryOperator.Value]
  ): Formula =
    new BinaryFormula(
      binaryQueue.dequeue(),
      leftChild.toFormula(unaryQueue, binaryQueue),
      rightChild.toFormula(unaryQueue, binaryQueue)
    )

}

case class UnaryNode(child: GenericFormula) extends GenericFormula {

  override def getBinaryCount(): Int =
    child.getBinaryCount()

  override def getUnaryCount(): Int =
    1 + child.getUnaryCount()

  override def getAtomCount(): Int =
    child.getAtomCount()

  override def toFormula(
      unaryQueue: Queue[UnaryOperator.Value],
      binaryQueue: Queue[BinaryOperator.Value]
  ): Formula =
    new UnaryFormula(
      unaryQueue.dequeue(),
      child.toFormula(unaryQueue, binaryQueue)
    )

}

case class AtomNode(atom: Atom) extends GenericFormula {

  override def getBinaryCount(): Int = 0

  override def getUnaryCount(): Int = 0

  override def getAtomCount(): Int = 1

  override def toFormula(
      unaryQueue: Queue[UnaryOperator.Value],
      binaryQueue: Queue[BinaryOperator.Value]
  ): Formula = atom

}
