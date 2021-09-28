package ttqg.logic

import scala.collection.mutable.ListBuffer

case class Valuation(v: Map[Atom, Boolean]) extends Map[Atom, Boolean]() {

  override def get(key: Atom): Option[Boolean] = Option(v(key))

  override def removed(key: Atom): Map[Atom, Boolean] = v.removed(key)

  override def updated[Boolean](key: Atom, value: Boolean): Map[Atom, Boolean] =
    Map[Atom, Boolean](key -> value)

  def iterator = v.iterator
  override def toString(): String = {
    val listBuffer = ListBuffer[String]()
    for ((a, b) <- v) {
      listBuffer += s"$a -> $b"
    }
    s"(${listBuffer.mkString(", ")})"
  }
}
