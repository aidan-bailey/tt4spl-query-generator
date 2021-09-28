package ttqg.logic

object TruthFunctional extends Enumeration {
  val True, False, Indeterminate = Value
}

object PropertyChecking {

  def getTruthFunctionalness(formula: Formula): TruthFunctional.Value =
    if (formula.isContradiction())
      TruthFunctional.False
    else if (formula.isTautology())
      TruthFunctional.True
    else TruthFunctional.Indeterminate

  def getTruthFunctionalness(
      knowledgeBase: KnowledgeBase
  ): TruthFunctional.Value =
    if (knowledgeBase.isContradiction())
      TruthFunctional.False
    else if (knowledgeBase.isTautology())
      TruthFunctional.True
    else TruthFunctional.Indeterminate

  def isTruthFunctionallyEquivalent(
      formula1: Formula,
      formula2: Formula
  ): Boolean =
    formula1.getModels().equals(formula2.getModels())

  def isConsistent(knowledgeBase: KnowledgeBase): Boolean =
    knowledgeBase.isConsistent()

  def isValid(
      premises: KnowledgeBase,
      conclusion: Formula
  ): Boolean =
    premises.getModels().forall(conclusion.evaluate(_))

  def entails(
      antecedent: KnowledgeBase,
      consequent: Formula
  ): Boolean =
    antecedent.entails(consequent)

}
