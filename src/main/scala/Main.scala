import ttqg.logic._
import ttqg.generation._
import scopt.OParser
import java.io.PrintWriter
import java.io.File

object Main extends App {

  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("ttqg"),
      head("ttqg", "1.0"),
      opt[Int]('n', "numCases")
        .action((x, c) => c.copy(numberOfCases = x))
        .valueName("<number>")
        .text("number of test cases")
        .validate(x =>
          if (x > 0) success
          else failure("Number of test cases must be >0")
        )
        .required(),
      opt[String]('p', "property")
        .valueName("<opt>")
        .action((x, c) =>
          c.copy(property = x match {
            case "consistent"    => Property.Consistent
            case "entailment"    => Property.Entailment
            case "validity"      => Property.Validity
            case "true"          => Property.TruthFunctionallyTrue
            case "false"         => Property.TruthFunctionallyFalse
            case "indeterminate" => Property.TruthFunctionallyIndeterminate
            case "equivalent"    => Property.TruthFunctionallyEquivalent
          })
        )
        .validate(x =>
          if (
            List[String](
              "consistent",
              "entailment",
              "validity",
              "true",
              "false",
              "indeterminate",
              "equivalent"
            ).contains(x)
          ) success
          else failure(s"Property option ${x} not valid")
        )
        .text(
          "{consistent, entailment, validity, true, false, indeterminate, equivalent}"
        )
        .required(),
      opt[Seq[String]]('a', "atoms")
        .valueName("<name>,<name>...")
        .action((x, c) =>
          c.copy(atoms =
            x.map(
              Atom(_)
            ).toSet
          )
        )
        .text("set of atom names")
        .required(),
      opt[String]('o', "out")
        .valueName("<filename>")
        .action((x, c) => c.copy(filename = x))
        .text("output file name"),
      opt[Int]('s', "size")
        .valueName("<int>")
        .action((x, c) => c.copy(size = x))
        .validate(x =>
          if (x > 0) success
          else failure("Set size must be >0")
        )
        .text(
          "size of knowledge base (consistent/true/false/indeterminate/equivalent - set size, entailment - knowledge base size, validity - premise set size) [default 1]"
        ),
      opt[Unit]("useAllAtoms")
        .action((_, c) => c.copy(useAllAtoms = true))
        .text("use every atom in each formula"),
      opt[Unit]("duplicates")
        .action((_, c) => c.copy(duplicateAtoms = true))
        .text("use duplicate atoms"),
      opt[Unit]("atomOnlyUnary")
        .action((_, c) => c.copy(atomOnlyUnary = true))
        .text("only atoms can have unary connectives"),
      opt[Seq[String]]('u', "unaryOperators")
        .valueName("<op>,<op>...")
        .validate(x => {
          val invalidOptions = x.filterNot(List[String]("not").contains(_))
          if (invalidOptions.isEmpty) success
          else
            failure(
              s"Unary operator(s) ${invalidOptions.map(a => s"'$a'").mkString(", ")} not valid"
            )
        })
        .action((x, c) =>
          c.copy(unaryOperators =
            x.map(_ match {
              case "not" => UnaryOperator.Negation
            }).toSet
          )
        )
        .text("{not} [default: all]"),
      opt[Seq[String]]('b', "binaryOperators")
        .valueName("<op>,<op>...")
        .validate(x => {
          val invalidOptions =
            x.filterNot(List[String]("and", "or", "implies", "iff").contains(_))
          if (invalidOptions.isEmpty) success
          else
            failure(
              s"Binary operator(s) ${invalidOptions.map(a => s"'$a'").mkString(", ")} not valid"
            )
        })
        .action((x, c) =>
          c.copy(binaryOperators =
            x.map(
              _ match {
                case "and"     => BinaryOperator.Conjunction
                case "or"      => BinaryOperator.Disjunction
                case "implies" => BinaryOperator.Implication
                case "iff"     => BinaryOperator.Equivalence
              }
            ).toSet
          )
        )
        .text("{and, or, implies, iff} [default: all]"),
      opt[Int]('r', "maxRecDepth")
        .valueName("<filename>")
        .action((x, c) => c.copy(maxRecDepth = x))
        .validate(x =>
          if (x > 0) success
          else failure("Max recursion depth must be >0")
        )
        .text("maximum recursion depth [default: 10000]")
    )
  }

// OParser.parse returns Option[Config]
  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      println("Generating Test Set")
      val result = config.property match {
        case Property.Consistent =>
          List
            .fill(config.numberOfCases)(
              KnowledgeBaseGenerator.generateConsistent(
                config.atoms,
                config.size,
                config.useAllAtoms,
                config.duplicateAtoms,
                config.atomOnlyUnary,
                config.unaryOperators,
                config.binaryOperators,
                config.maxRecDepth
              )
            )
            .filter(_.isDefined)
            .map(_.get)
        case Property.Entailment =>
          List
            .fill(config.numberOfCases)(
              KnowledgeBaseGenerator.generateEntailment(
                config.atoms,
                config.size,
                config.useAllAtoms,
                config.duplicateAtoms,
                config.atomOnlyUnary,
                config.unaryOperators,
                config.binaryOperators,
                config.maxRecDepth
              )
            )
            .filter(_.isDefined)
            .map(_.get)
        case Property.Validity =>
          List
            .fill(config.numberOfCases)(
              KnowledgeBaseGenerator.generateArgument(
                config.atoms,
                config.size,
                config.useAllAtoms,
                config.duplicateAtoms,
                config.atomOnlyUnary,
                config.unaryOperators,
                config.binaryOperators,
                config.maxRecDepth
              )
            )
            .filter(_.isDefined)
            .map(_.get)
        case Property.TruthFunctionallyEquivalent =>
          List
            .fill(config.numberOfCases)(
              KnowledgeBaseGenerator.generateEquivalent(
                config.atoms,
                config.size,
                config.duplicateAtoms,
                config.atomOnlyUnary,
                config.unaryOperators,
                config.binaryOperators,
                config.maxRecDepth
              )
            )
            .filter(_.isDefined)
            .map(_.get)
        case Property.TruthFunctionallyTrue => {
          List
            .fill(config.numberOfCases)(
              KnowledgeBaseGenerator.generateTruthFunctional(
                TruthFunctional.True,
                config.atoms,
                config.size,
                config.useAllAtoms,
                config.duplicateAtoms,
                config.atomOnlyUnary,
                config.unaryOperators,
                config.binaryOperators,
                config.maxRecDepth
              )
            )
            .filter(_.isDefined)
            .map(_.get)
        }
      }
      println("Test set generated")
      if (config.filename.isEmpty())
        println(result.mkString("\n"))
      else {
        val pw = new PrintWriter(new File(config.filename))
        pw.write(result.mkString("\n"))
        pw.close()
        println("Written to " + config.filename)
      }
    case _ =>
    // arguments are bad, error message will have been displayed
  }

}
