package rewriter

import scala.util.parsing.combinator._
import java.io.FileReader

import rewriter.Rewriter._
import rewriter.TRS._

object Parser extends JavaTokenParsers{

  //TODO: think if matching should be done with stringliteral or other
  def signature: Parser[Map[String,Operation]] =
     rep(constant | function | operator) ^^ { Map() ++ _}

  def constant: Parser[(String,Operation)] =
    "const" ~> (ident | wholeNumber) ^^ {
      name => (name,constOp(name))
    }

  def function: Parser[(String,Operation)] =
    "function" ~> ident ~ wholeNumber ^^ {
      case name ~ arity => (name,FunOp(name,arity.toInt))
    }

  def operator: Parser[(String,Operation)] =
    (("infixl" | "infixr" | "prefix" | "postfix") ~ stringLiteral ~ wholeNumber) ^^ {
      case "infixl" ~ name ~ priority => (name,InfixOp(name,Assoc.AssocLeft,priority.toInt))
      case "infixr" ~ name ~ priority => (name,InfixOp(name,Assoc.AssocRight,priority.toInt))
      case "prefix" ~ name ~ priority => (name,PrefixOp(name,priority.toInt))
      case "postfix" ~ name ~ priority => (name,PostfixOp(name,priority.toInt))
    }

  def rules: Parser[List[Rule]] = rep(rule) ^^ { List() ++ _}

  def rule: Parser[Rule] = "rule" ~ term ~ "->" ~ term ^^ {
    case "rule" ~ t1 ~ "->" ~ t2 => Rule(t1,t2)
  }

  //TODO: parse with the rigth encoding
  def term: Parser[Term] =
    ident ^^ {
      name => Var(name)
    } | {stringLiteral ~ "(" ~ repsep(term,",") ~ ")"} ^^ {
      case name ~ "(" ~ ts ~ ")" => Fun(name,List() ++ ts)
    }

  def parser: Parser[(Map[String,Operation],List[Rule])] =
    ("section signature" ~> signature <~ "end signature") ~
      ("section rules" ~> rules <~ "end rules") ^^ {
      case s ~ rs => (s,rs)
    }

  def parseTRSFile(fileName: String): (Map[String,Operation],List[Rule]) = {
    val reader = new FileReader(fileName)
    //TODO: transform(terms)
    //TODO: error handling
    parseAll(parser,reader) match{
      case Success(r,_) =>
        checkRules(r._2) //TODO: check that rules are well constructed
        (r._1,r._2)
      case Failure(msg,_) =>
        println("Parser failed with output: " + msg); (Map(),List())
    }
  }

  def consoleTerm: Parser[Term] = ("term " | "rewrite " | "trace " | "normalise " | "normalise_trace ") ~> term


  // Handles cases unify and match
  def consoleTerms: Parser[(Term,Term)] = (
    ("unify " ~> term) ~ (" and " ~> term) ^^ { case t1 ~ t2 => (t1,t2) }
    | ("match " ~> term) ~ (" to " ~> term) ^^ { case t1 ~ t2 => (t1,t2) }
  )

  def checkRules(rs: List[Rule]): Boolean =
  def checkTerm(t: Term): Boolean = {

  }
}
