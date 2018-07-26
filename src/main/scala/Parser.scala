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
    "const" ~> stringLiteral ^^ {
      name => (name,constOp(name))
    }

  def function: Parser[(String,Operation)] =
    "function" ~> stringLiteral ~ wholeNumber ^^ {
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

  //TODO: change this return type
  def parseTRSFile(fileName: String): (Map[String,Operation],List[Rule]) = {
    val reader = new FileReader(fileName)
    //TODO: transform(terms)
    //TODO: error handling
    parseAll(parser,reader) match{
      case Success(r,_) =>
        if(checkRules(r._1,r._2)) (r._1,r._2) else (Map(),List())
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

  //There is no need to check there are no two functions with same name
  def checkRules(ops: Map[String,Operation],rs: List[Rule]): Boolean = {
    val trs = new TRS(ops,rs)
    rs.forall(r => checkTerm(trs,r.left) && checkTerm(trs,r.right))
  }

  //TODO: improve check
  def checkTerm(trs: TRS,t: Term): Boolean = t match {
    case Var(_) => true
    case op@Fun(n,args) =>
      args.forall(checkTerm(trs,_)) && trs.ops.keySet.contains(n) && opArity(trs.ops(n)) == args.length
  }
}
















