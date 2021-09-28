package ttqg.test

import org.scalatest.funspec.AnyFunSpec
import ttqg.logic._

class PropertyCheckingSpec extends AnyFunSpec {
  describe("Truth-functional True") {
    it("Test 1") {
      val formula = Implies(Atom("L"), Or(Atom("K"), Atom("L")))
      assert(
        PropertyChecking
          .getTruthFunctionalness(formula)
          .equals(TruthFunctional.True)
      )
    }
  }

  describe("Truth-functional Consistency") {
    it("Test 1") {
      val k = new KnowledgeBase(
        Implies(And(Atom("W"), Atom("Y")), Atom("H")),
        Implies(Atom("W"), Implies(Atom("Y"), Atom("H")))
      )
      assert(
        PropertyChecking.isConsistent(k)
      )
    }
  }

  describe("Truth-functional Equivalence") {
    it("Test 1") {
      val formula1 = Implies(And(Atom("W"), Atom("Y")), Atom("H"))
      val formula2 = Implies(Atom("W"), Implies(Atom("Y"), Atom("H")))
      assert(
        PropertyChecking.isTruthFunctionallyEquivalent(formula1, formula2)
      )
    }
  }

  describe("Truth-functional Validity") {
    it("Test 1") {
      val premise = new KnowledgeBase(
        Iff(Atom("F"), Atom("G")),
        Or(Atom("F"), Atom("G"))
      )
      val conclusion = And(Atom("F"), Atom("G"))
      PropertyChecking.isValid(premise, conclusion)
    }
  }

  describe("Truth-functional Entailment") {
    it("Test 1") {
      val k = new KnowledgeBase(
        Or(Atom("K"), Atom("J")),
        Not(Or(Atom("K"), Atom("J")))
      )
      assert(PropertyChecking.entails(k, Atom("K")))
    }
  }

}
