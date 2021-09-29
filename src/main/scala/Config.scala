import ttqg.logic.Atom
import ttqg.logic.BinaryOperator
import ttqg.logic.UnaryOperator
import ttqg.logic.TruthFunctional
import ttqg.generation.KnowledgeBaseGenerator

object Property extends Enumeration {
  val Consistent, Entailment, Validity, TruthFunctionallyTrue,
      TruthFunctionallyFalse, TruthFunctionallyIndeterminate,
      TruthFunctionallyEquivalent = Value
}

case class Config(
    property: Property.Value = Property.Consistent,
    numberOfCases: Int = 1,
    size: Int = 1,
    atoms: Set[Atom] = Set(),
    filename: String = "",
    antiTest: Boolean = false,
    useAllAtoms: Boolean = false,
    duplicateMax: Int = 0,
    literalsOnly: Boolean = false,
    unaryOperators: Set[UnaryOperator.Value] = UnaryOperator.values,
    binaryOperators: Set[BinaryOperator.Value] = BinaryOperator.values,
    maxRecDepth: Int = 1000
)
