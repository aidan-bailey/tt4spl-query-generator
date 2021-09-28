import ttqg.logic.Atom
import ttqg.logic.BinaryOperator
import ttqg.logic.UnaryOperator
import ttqg.logic.TruthFunctional
import ttqg.generation.KnowledgeBaseGenerator

object Property extends Enumeration {
  val Consistent, Entailment, Validity, TruthFunctionallyTrue,
      TruthFunctionallyFalse, TruthFunctionallyIndeterminate,
      TruthFunctionallyEquivalent = Value
  def convert(property: Property.Value) = {
    def truthfunctionally(p: TruthFunctional.Value) = {
      KnowledgeBaseGenerator.generateTruthFunctional(
        p,
        _,
        _,
        _,
        _,
        _,
        _,
        _,
        _
      )
    }
    property match {
      case Consistent =>
        KnowledgeBaseGenerator.generateConsistent(_, _, _, _, _, _, _, _)
      case Entailment =>
        KnowledgeBaseGenerator.generateEntailment(_, _, _, _, _, _, _, _)
      case Validity =>
        KnowledgeBaseGenerator.generateArgument(_, _, _, _, _, _, _, _)
      case TruthFunctionallyEquivalent =>
        KnowledgeBaseGenerator.generateEquivalent(_, _, _, _, _, _, _)
      case TruthFunctionallyTrue =>
        truthfunctionally(TruthFunctional.True)
      case TruthFunctionallyFalse =>
        truthfunctionally(TruthFunctional.False)
      case TruthFunctionallyIndeterminate =>
        truthfunctionally(TruthFunctional.Indeterminate)
    }
  }
}

case class Config(
    property: Property.Value = Property.Consistent,
    numberOfCases: Int = 1,
    size: Int = 1,
    atoms: Set[Atom] = Set(),
    filename: String = "",
    useAllAtoms: Boolean = false,
    duplicateAtoms: Boolean = false,
    atomOnlyUnary: Boolean = false,
    unaryOperators: Set[UnaryOperator.Value] = UnaryOperator.values,
    binaryOperators: Set[BinaryOperator.Value] = BinaryOperator.values,
    maxRecDepth: Int = 10000
)
