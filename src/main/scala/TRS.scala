package rewriter

import rewriter.Rewriter._

object TRS {

  //TODO: originally there was a Assoc None value
  object Assoc extends Enumeration {
    type Assoc = Value
    val AssocLeft,AssocRight = Value
  }
  type Signature = Map[String,Operation]

  //TODO: we keep this operation hierarchy for converting from the outside and printing
  sealed trait Operation{ def print: String }
  case class FunOp(n: String,i: Integer) extends Operation{
    def print: String = if(i == 0) "const " ++ n else{"function " ++ n ++ " " ++ i.toString()}
  }
  case class PrefixOp(n: String,i: Integer) extends Operation{ def print: String = "prefix " ++ n ++ " " ++ i.toString() }
  case class PostfixOp(n: String,i: Integer) extends Operation{ def print: String = "postfix " ++ n ++ " " ++ i.toString() }
  case class InfixOp(n: String,a: Assoc.Assoc,i: Integer) extends Operation{ def print: String = "infixl " ++ n ++ " " ++ i.toString() }

  case class TRS(ops: List[(String,Operation)],rs: List[Rule])

  def constOp(s: String) = FunOp(s,0)

    /*
  import Text.ParserCombinators.Parsec.Expr (Assoc(AssocLeft,AssocRight))

  instance Eq Operation where
  FunOp s1 a1 == FunOp s2 a2 = (s1 == s2) && (a1 == a2)
  PrefixOp s1 _ == PrefixOp s2 _ = (s1 == s2)
  InfixOp s1 _ _ == InfixOp s2 _ _ = (s1 == s2)
  PostfixOp s1 _ == PostfixOp s2 _ = (s1 == s2)
  FunOp s1 a1 == PrefixOp s2 _ = (s1 == s2) && (a1 == 1)
  FunOp s1 a1 == InfixOp s2 _ _ = (s1 == s2) && (a1 == 2)
  FunOp s1 a1 == PostfixOp s2 _ = (s1 == s2) && (a1 == 1)
  PrefixOp s1 _ == FunOp s2 a2 = (s1 == s2) && (a2 == 1)
  InfixOp s1 _ _ == FunOp s2 a2 = (s1 == s2) && (a2 == 2)
  PostfixOp s1 _ == FunOp s2 a2 = (s1 == s2) && (a2 == 1)
  PrefixOp s1 _ == PostfixOp s2 _ = (s1 == s2)
  PostfixOp s1 _ == PrefixOp s2 _ = (s1 == s2)
  _ == _ = False

  show (InfixOp s AssocLeft p) = "infixl " ++ s ++ " " ++ show p
  show (InfixOp s AssocRight p) = "infixr " ++ s ++ " " ++ show p

  */
/*
    def isWellformed(s: Signature,t: Term): Boolean = t match{
      case Var(_) => true
      case Fun(f,args) => s.get(f) match{
        case None => false
        case Some(arities) => (arities contains args.length) && args.forall(isWellformed(s,_))
      }
    }
*/
    def opName(op: Operation): String = op match{
      case FunOp(s,_) => s
      case PrefixOp(s,_) => s
      case InfixOp(s,_,_) => s
      case PostfixOp(s,_) => s
    }

    def opArity(op: Operation): Integer = op match{
      case FunOp(_,a) => a
      case InfixOp(_,_,_) => 2
      case _ => 1
    }

    def opPrio(op: Operation): Integer = op match{
      case PrefixOp(_,p) => p
      case InfixOp(_,_,p) => p
      case PostfixOp(_,p) => p
    }

    def opIsOperator(op: Operation): Boolean = op match{
      case FunOp(_,_) => false
      case _ => true
    }

    def buildOpMap(ops: List[Operation]): Map[(String,Integer),Operation] = {
      def f(m:Map[(String,Integer),Operation],op: Operation): Map[(String,Integer),Operation] =
        m + ((opName(op),opArity(op)) -> op)
      ops.foldLeft (Map.empty: Map[(String,Integer),Operation]) (f)
    }

}
