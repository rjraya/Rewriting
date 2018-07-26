package rewriter

import rewriter.TRS._
import rewriter.Rewriter._

object Printer {

  def escapeArg(p: Integer,t: Term,m: Map[(String,Integer),Operation]) = t match{
    case Var(x) => x
    case t@Fun(f,args) => m.get((f,args.length)) match{
      case Some(FunOp(_,_)) => prettyPrintRec(t,m)
      case Some(op) =>
        if(opPrio(op) <= p) "(" ++ prettyPrintRec(t,m) ++ ")" else prettyPrintRec(t,m)
      case _ => prettyPrintRec(t,m)
    }
  }

  def prettyPrintRec(t: Term,m: Map[(String,Integer),Operation]): String = t match{
    case Var(x) => x
    case Fun(f,Nil) => f
    case Fun(f,args) => m.get(f,args.length) match{
      case Some(PrefixOp(_,p)) =>
        f ++ escapeArg(p,args.head,m)
      case Some(InfixOp(_,Assoc.AssocLeft,p)) =>
        escapeArg(p-1,args.head,m) ++ " " ++ f ++ " " ++ escapeArg(p,args.last,m)
      case Some(InfixOp(_,Assoc.AssocRight,p)) =>
        escapeArg(p,args.head,m) ++ " " ++ f ++ " " ++ escapeArg(p-1,args.last,m)
      case _ =>
        f ++ "(" ++ args.map(prettyPrintRec(_,m)).mkString(",") ++ ")"
    }
  }

  def prettyPrint(ops: List[Operation], t: Term): String =
    prettyPrintRec(t,buildOpMap(ops))

  def prettyPrintRule(ops: List[Operation],r: Rule): String = prettyPrint(ops,r.left) ++ " → " ++ prettyPrint(ops,r.right)

  def prettyPrintTrace(ops: List[Operation],tr: Trace): String = tr.termList.reverse.map(prettyPrint(ops,_)).mkString("\n  → ")

  def prettyPrintSubst(ops: List[Operation],s: Subst): String = s.toList.map{
    case (x: String,t: Term) => x ++ " ↦ " ++ prettyPrint(ops,t)
  }.mkString("\n")


}
