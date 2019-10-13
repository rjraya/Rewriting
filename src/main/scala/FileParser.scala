package rewriter

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._


import rewriter.Rewriter._
import rewriter.TRS._

object FileParser extends StandardTokenParsers{
  lexical.reserved ++= List("rule","const","function","infixl","infixr","prefix","postfix","begin", "end","signature","rules")
  lexical.delimiters ++= List("\n","(",",",")","->")

  def parser: Parser[(List[(String,Operation)],List[Rule])] =
    ("begin" ~ "signature" ~ signature ~ "end" ~ "signature" ~
      "begin" ~ "rules" ~ rules ~ "end" ~ "rules") ^^ { case _ ~ _ ~ s ~ _ ~ _ ~ _ ~ _ ~ rs ~ _ ~ _=> (s,rs) }

  // Parse signature section
  def signature: Parser[List[(String,Operation)]] =
     rep(constant | function | operator)

  def constant: Parser[(String,Operation)] =
    ("const" ~ stringLit) ^^ { case _ ~ name => (name,constOp(name)) }

  def function: Parser[(String,Operation)] =
    ("function" ~ stringLit ~ numericLit) ^^ { case _ ~ name ~ arity => (name,FunOp(name,arity.toInt)) }

  def operator: Parser[(String,Operation)] =
    (("infixl" | "infixr" | "prefix" | "postfix") ~ stringLit ~ numericLit) ^^ {
      case "infixl" ~ name ~ priority => (name,InfixOp(name,Assoc.AssocLeft,priority.toInt))
      case "infixr" ~ name ~ priority => (name,InfixOp(name,Assoc.AssocRight,priority.toInt))
      case "prefix" ~ name ~ priority => (name,PrefixOp(name,priority.toInt))
      case "postfix" ~ name ~ priority => (name,PostfixOp(name,priority.toInt))
    }

  //Parse rules section
  def rules: Parser[List[Rule]] = rep(rule)

  def rule: Parser[Rule] =
    "rule" ~ term ~ "->" ~ term ^^ { case _ ~ t1 ~ _ ~ t2 => Rule(t1,t2)}

  //TODO: parse with the right encoding
  def term: Parser[Term] =
    ident ^^ { Var } | {stringLit ~ "(" ~ repsep(term,",") ~ ")"} ^^ {
      case name ~ _ ~ ts ~ _ => Fun(name,ts)
    }

  def parseTRSFile(fileName: String): (List[(String,Operation)],List[Rule]) = {
    val lines = scala.io.Source.fromFile(fileName).mkString

    val tokens = new lexical.Scanner(lines)

    phrase(parser)(tokens) match {
      case Success(res,next) =>
        if(checkRules(res._1,res._2)) (res._1,res._2) else (List(),List())
      case Failure(msg,_) =>
        println("Parser failed with output: " + msg); (List(),List())
    }
  }

  //TODO: There is no need to check there are no two functions with same name
  def checkRules(ops: List[(String,Operation)],rs: List[Rule]): Boolean = {
    val trs = new TRS(ops,rs)
    rs.forall(r => checkTerm(trs,r.left) && checkTerm(trs,r.right))
  }

  //TODO: improve check
  def checkTerm(trs: TRS,t: Term): Boolean = t match {
    case Var(_) => true
    case Fun(n,args) =>
      args.forall(checkTerm(trs,_)) && trs.ops.map{ _._1 }.contains(n) && opArity(trs.ops(n)._2) == args.length
  }

}
















