import ttqg.logic._
import ttqg.generation._
import scopt.OParser
import java.io.PrintWriter
import java.io.File
import scala.concurrent.Await
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.util.Success

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
        .valueName("<size>")
        .action((x, c) => c.copy(size = x))
        .validate(x =>
          if (x > 0) success
          else failure("The value <size> must be >0")
        )
        .text(
          "size of knowledge base (consistent/true/false/indeterminate/equivalent - set size, entailment - knowledge base size, validity - premise set size) [default 1]"
        ),
      opt[Unit]("antiTest")
        .abbr("at")
        .action((_, c) => c.copy(antiTest = true))
        .text("generate negative test cases"),
      opt[Unit]("useAllAtoms")
        .abbr("all")
        .action((_, c) => c.copy(useAllAtoms = true))
        .text("use every atom in each formula"),
      opt[Int]('d', "duplicateMax")
        .action((x, c) => c.copy(duplicateMax = x))
        .valueName("<duplicateMax>")
        .text("maximum number of duplicates")
        .validate(x =>
          if (x > -1) success
          else failure("The value <duplicateMax> must be >0")
        ),
      opt[Unit]("literalsOnly")
        .abbr("literals")
        .action((_, c) => c.copy(literalsOnly = true))
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
      println("Generating test set...")
      val result = config.property match {
        case Property.Consistent =>
          List
            .fill(config.numberOfCases)(
              KnowledgeBaseGenerator.generateConsistent(
                config.antiTest,
                config.atoms,
                config.size,
                config.duplicateMax,
                config.useAllAtoms,
                config.literalsOnly,
                config.unaryOperators,
                config.binaryOperators,
                config.maxRecDepth
              )
            )
            .map(Await.result(_, Duration(5, TimeUnit.MINUTES)))
            .filter(_.isDefined)
            .map(_.get)
        case Property.Entailment =>
          List
            .fill(config.numberOfCases)(
              KnowledgeBaseGenerator.generateEntailment(
                config.antiTest,
                config.atoms,
                config.size,
                config.duplicateMax,
                config.useAllAtoms,
                config.literalsOnly,
                config.unaryOperators,
                config.binaryOperators,
                config.maxRecDepth
              )
            )
            .map(Await.result(_, Duration(5, TimeUnit.MINUTES)))
            .filter(_.isDefined)
            .map(_.get)
        case Property.Validity =>
          List
            .fill(config.numberOfCases)(
              KnowledgeBaseGenerator.generateArgument(
                config.antiTest,
                config.atoms,
                config.size,
                config.duplicateMax,
                config.useAllAtoms,
                config.literalsOnly,
                config.unaryOperators,
                config.binaryOperators,
                config.maxRecDepth
              )
            )
            .map(Await.result(_, Duration(5, TimeUnit.MINUTES)))
            .filter(_.isDefined)
            .map(_.get)
        case Property.TruthFunctionallyEquivalent =>
          List
            .fill(config.numberOfCases)(
              KnowledgeBaseGenerator.generateEquivalent(
                config.antiTest,
                config.atoms,
                config.size,
                config.duplicateMax,
                config.literalsOnly,
                config.unaryOperators,
                config.binaryOperators,
                config.maxRecDepth
              )
            )
            .map(Await.result(_, Duration(5, TimeUnit.MINUTES)))
            .filter(_.isDefined)
            .map(_.get)
        case Property.TruthFunctionallyTrue => {
          List
            .fill(config.numberOfCases)(
              KnowledgeBaseGenerator.generateTruthFunctional(
                config.antiTest,
                TruthFunctional.True,
                config.atoms,
                config.size,
                config.duplicateMax,
                config.useAllAtoms,
                config.literalsOnly,
                config.unaryOperators,
                config.binaryOperators,
                config.maxRecDepth
              )
            )
            .map(Await.result(_, Duration(5, TimeUnit.MINUTES)))
            .filter(_.isDefined)
            .map(_.get)
        }
        case Property.TruthFunctionallyFalse => {
          List
            .fill(config.numberOfCases)(
              KnowledgeBaseGenerator.generateTruthFunctional(
                config.antiTest,
                TruthFunctional.False,
                config.atoms,
                config.size,
                config.duplicateMax,
                config.useAllAtoms,
                config.literalsOnly,
                config.unaryOperators,
                config.binaryOperators,
                config.maxRecDepth
              )
            )
            .map(Await.result(_, Duration(5, TimeUnit.MINUTES)))
            .filter(_.isDefined)
            .map(_.get)
        }
        case Property.TruthFunctionallyIndeterminate => {
          List
            .fill(config.numberOfCases)(
              KnowledgeBaseGenerator.generateTruthFunctional(
                config.antiTest,
                TruthFunctional.Indeterminate,
                config.atoms,
                config.size,
                config.duplicateMax,
                config.useAllAtoms,
                config.literalsOnly,
                config.unaryOperators,
                config.binaryOperators,
                config.maxRecDepth
              )
            )
            .map(Await.result(_, Duration(5, TimeUnit.MINUTES)))
            .filter(_.isDefined)
            .map(_.get)
        }
      }
      println("Test set generated")
      if (config.filename.isEmpty())
        if (result.isEmpty)
          println("Failed to find test cases")
        else
          println(result.mkString("\n"))
      else {
        val pw = new PrintWriter(new File(config.filename))
        println("Written to " + config.filename)
        pw.write(result.mkString("\n"))
        pw.close()
        println("Written to " + config.filename)
      }
    case _ =>
    // arguments are bad, error message will have been displayed
  }

}
