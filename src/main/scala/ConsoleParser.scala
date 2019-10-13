package rewriter
/*
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.collection.immutable._


import rewriter.Rewriter._
import rewriter.TRS._

object ConsoleParser extends StandardTokenParsers{
  lexical.reserved ++= List("term","rewrite","trace","normalise","normalise_trace")
  lexical.delimiters ++= List("\n")

  //TODO: parse with the right encoding
  def term: Parser[Term] =
    ident ^^ { Var } | {stringLit ~ "(" ~ repsep(term,",") ~ ")"} ^^ {
      case name ~ _ ~ ts ~ _ => Fun(name,ts)
    }

  def consoleTerm: Parser[Term] =
    ("term" | "rewrite" | "trace" | "normalise" | "normalise_trace") ~ term

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
*/