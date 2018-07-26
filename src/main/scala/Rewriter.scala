package rewriter

object Rewriter {
  //TODO: Eq, Ord
  //TODO: check if string comparaison is the right way to implement Ord
  sealed trait Term extends Ordered[Term]{
    def print: String
  }
  case class Var(name: String) extends Term{
    def print: String = name

    override def compare(that: Term): Int = this.name.compare(that match{
      case Var(n) => n
      case Fun(f,_) => f
    })
  }
  case class Fun(name: String, args: List[Term]) extends Term{
    def print: String = name + args.mkString("(",",",")")

    override def compare(that: Term): Int = this.name.compare(that match{
      case Var(n) => n
      case Fun(f,_) => f
    })
  }

  case class Rule(left: Term,right: Term){ def print: String = left.print ++ " → " ++ right.print }

  //TODO: check if equals is necessary or already provided by case class as is
  //TODO: check a hashcode method is not needed
  //TODO: check if method equals is correctly implemented
  case class Trace(termList: List[Term]) extends Equals with Ordered[Trace]{
    def print: String = termList.reverse.map(t => t.print).mkString("\n ->")
    //TODO: match not exhaustive _ _
    override def equals(o: scala.Any): Boolean = (this,o) match{
      case (Trace(Nil),Trace(Nil)) => true
      case (Trace(Nil),_) => false
      case (_,Trace(Nil)) => false
      case (Trace(l1),Trace(l2)) => l1.head == l2.head
    }

    override def compare(that: Trace): Int = (this,that) match{
      case (Trace(Nil),Trace(Nil)) => 0
      case (Trace(Nil),_) => -1
      case (_,Trace(Nil)) => 1
      case (Trace(l1),Trace(l2)) => l1.head.compare(l2.head)
    }
  }

  //TODO: we may want this to be a hashmap
  type Subst = Map[String,Term]

  def variablesHashSetRec(t: Term,s: Set[String]): Set[String] = t match{
    case Var(x) => s + x
    case Fun(_,args) =>  args.foldLeft(s){(acc: Set[String],ft: Term) => variablesHashSetRec(ft,acc)}
  }

  // TODO: think if this should be a hashset
  // TODO: https://stackoverflow.com/questions/26901866/scala-default-set-implementation
  def variablesHashSet(t: Term): Set[String] =
  variablesHashSetRec(t,Set.empty)

  def variables(t: Term): List[String] = variablesHashSet(t).toList

  // Tests if term t contains variable with name s
  def containsVar(s: String,t: Term): Boolean = t match{
    case Var(n) => s == n
    case Fun(_,args) => args exists{ containsVar(s,_) }
  }

  // Substitutes variable with name n in term t by s
  def substVar(n: String,s: Term,t: Term): Term = t match{
    case Var(m) => if(n == m) s else t
    case Fun(f,args) => Fun(f,args map{ substVar(n,s,_) })
  }

  //TODO: Probably this should be an object with apply method
  //applies substituion s to term t
  def applySubst(s: Subst,t: Term): Term = t match{
    case Var(n) => s.getOrElse(n,t)
    case Fun(f,args) => Fun(f,args map{ applySubst(s,_) })
  }

  // updates each entry of s and adds substitution n to t
  def updateSubst(n: String,t: Term,s: Subst): Subst =
    (s map { case (m,l) => (m,substVar(n,t,l)) }) + (n -> t)


  // Renames all vars in t according to g
  def renameVars(g: String => String,t: Term): Term = t match{
    case Var(n) => Var(g(n))
    case Fun(f,args) => Fun(f,args map{ renameVars(g,_) })
  }

  //TODO: check if parameter of isGround needs to be specified
  def isGround(t: Term): Boolean = t match{
    case Var(_) => false
    case Fun(_,args) => args forall{ isGround }
  }

  //TODO: this should be modelled as an exception?
  //TODO: Clash derives from Eq
  //TODO: introduce pretty printing such in
  //TODO: https://github.com/rjraya/toolc/blob/master/src/main/scala/toolc/ast/Printer.scala
  sealed trait UnificationError{
    def print: String
  }
  case class OccursCheck(n: String,t: Term) extends UnificationError{
    override def print: String = "occurs check failed at " ++ n ++ " != " ++ t.print
  }
  case class Clash(t: Term,l: Term) extends UnificationError{
    override def print: String = "class at " ++ t.print ++ " != " ++ l.print
  }

  def matchRec(p: (Term,Term),s: Subst): Either[UnificationError,Subst] = (p._1,p._2) match{
    case (fs@Fun(f,fargs),gs@Fun(g,gargs)) =>
      if((f != g) || (fargs.length != gargs.length)) Left(Clash(fs,gs))
      else (fargs zip gargs).foldLeft(Right(s): Either[UnificationError,Subst])(
        (sp: Either[UnificationError,Subst],tp: (Term,Term)) => sp match {
          case Left(_) => sp
          case Right(rs) => matchRec(tp,rs)
        }
      )
    case (Var(x),t) => s.get(x) match{
      case None => Right((s: Subst) + (x -> t))
      case Some(ot) => if(t == ot) Right(s) else Left(Clash(t,ot))
    }
    case (st,t@Var(_)) => Left(Clash(st,t))
  }

  //TODO: write documentation
  def mmatch(p: (Term,Term)): Either[UnificationError,Subst] =
    matchRec(p,Map.empty)

  def matchMaybe(p: (Term,Term)): Option[Subst] = mmatch(p) match{
    case Left(_) => None
    case Right(s) => Some(s)
  }

  // applies x = t to the given constraint
  //TODO: what is a constraint
  def updateConstr(n: String,t: Term, l: (Term,Term)): (Term,Term) =
  (substVar(n,t,l._1),substVar(n,t,l._2))

  //TODO: return type is 'Either UnificationError Subst'
  //TODO: this is on page 80 of the book
  // Finds the most general substitution that makes the lhs and rhs of all
  // constrainsts in l equal
  def unifyRec(l: List[(Term,Term)], s: Subst): Either[UnificationError,Subst] = l match{
    case Nil => Right(s)
    case (Var(x),t@Var(y)) :: constrs =>
      if(x == y) unifyRec(constrs,s)
      else if(x.length < y.length) unifyRec((Var(y),Var(x)) :: constrs,s)
      else unifyRec(constrs map{ updateConstr(x,t,_) },updateSubst(x,t,s))
    case (Var(x),t) :: constrs =>
      if(containsVar(x,t)) Left(OccursCheck(x,t))
      else unifyRec(constrs map{ updateConstr(x,t,_) },updateSubst(x,t,s))
    case (t,Var(x)) :: constrs =>
      unifyRec((Var(x),t) :: constrs,s)
    case (ft@Fun(f,fargs), gt@Fun(g,gargs)) :: constrs =>
      if((f != g) || (fargs.length != gargs.length)) Left(Clash(ft,gt))
      else unifyRec((fargs zip gargs) ++ constrs,s)
  }

  // Finds the most general unifier for terms s,t, that is, a substitution
  // such that applied to them yields the same term and such that any other
  // substitution unifying them is a composition of this one.
  //
  // If success, Left substitution is returned else Right error indicating
  // the source of the error that caused the unification to fail
  def unify(p: (Term,Term)): Either[UnificationError,Subst] =
  unifyRec(List(p),Map.empty)

  // Same as unify but returning Option instead of Either
  // TODO: check whether this is the behaviour intended in the Left case
  // TODO: for (const Nothing)
  def unifyOption(p: (Term,Term)): Option[Subst] =
  unify(p) match{
    case Left(_) => None
    case Right(s) => Some(s)
  }

  def unifyAllRec(l: List[Term],s: Subst): Either[UnificationError,Subst] = l match{
    case t1 :: t2 :: ts =>
      unifyRec(List((applySubst(s,t1) , applySubst(s,t2))),s) match{
        case Right(us) => unifyAllRec(t1 :: ts,us)
        case Left(e) => Left(e)
      }
    case _ => Right(s)
  }

  // Finds the most general unifier for all terms in l
  def unifyAll(l: List[Term]): Either[UnificationError,Subst] =
    unifyAllRec(l,Map.empty)

  // Given rule r and list of terms [t1,...,tn] picks i, rewrites ti -> ti' and
  // returns [t1,...,ti',...,tn]
  def rewriteOneWith(r: Rule,ts: List[Term]): List[List[Term]] = {
    def f(t: Term,p: (List[Term],List[List[Term]])): (List[Term],List[List[Term]]) = p match{
      case (tl,acc) =>
        (t :: tl, (rewriteWith(r,t) map {_ :: tl}) ++ (acc map { t +: _ }))
    }

    ts.foldRight (List[Term](),List[List[Term]]()) (f) ._2
  }

  // Performs a single rewrite on t with r
  //TODO: check if this is really what fmap does
  def rewriteWith(r: Rule,t: Term): List[Term] = r match{ case Rule(lt,rt) =>
    (matchMaybe((lt,t)) match{
      case None => Nil
      case Some(s) => List(applySubst(s,rt))
    }) ++
      (t match{
        case Var(_) => Nil
        case Fun(f,args)=> rewriteOneWith(r,args).map(Fun(f,_))
      })
  }

  // Performs a single rewrite step on t with any of the rules of rs
  def rewrite(rs: List[Rule],t: Term): List[Term] = rs flatMap (rewriteWith(_,t))

  //TODO: see if foldl can be substituted by fold
  //TODO: https://stackoverflow.com/questions/7000149/why-does-fold-have-the-following-type-in-scala
  //TODO: see if fresh parameters are needed for foldleft
  def rewriteStarRec(rs:List[Rule], ts: Set[Term]): Set[Term] = {
    def f(tp: (Boolean,Set[Term]),t: Term): (Boolean,Set[Term]) = rewrite(rs,t) match{
      case Nil => (tp._1,tp._2 + t)
      case ts2 => (true,ts2.foldLeft (tp._2) { case (fts,ft) => fts + ft })
    }

    ts.foldLeft((false,Set.empty: Set[Term]))(f) match {
      case (true,mts) => rewriteStarRec(rs,mts)
      case (false,s) => s
    }
  }

  // Returns all terms reachable from t with rs
  // Equivalently, enumerates the reflexive transitive relation ->*

  def rewriteStar(rs: List[Rule],t: Term): List[Term] =
    rewriteStarRec(rs,Set(t)).toList

  //TODO: see if fold can be used in spite of the type
  def rewriteTraceRec(rs: List[Rule], ts: Set[Trace]): Set[Trace] = {
    def f(rs: List[Rule], tr: List[Term],b: Boolean,fts: Set[Trace]): (Boolean,Set[Trace]) =
      rewrite(rs,tr.head) match{
        case Nil => (b,fts + Trace(tr))
        case cfts => (true,cfts.foldLeft(fts){(mts,mt) => mts + Trace(mt :: tr) })
      }
    //(True, foldl (\ts' t' -> S.insert (Trace (t':tr)) ts') ts' ts'')
    ts.foldLeft((false, Set.empty: Set[Trace]))((p: (Boolean,Set[Trace]),t: Trace) => f(rs,t.termList,p._1,p._2)) match {
      case (true, cts: Set[Trace]) => rewriteTraceRec(rs, cts)
      case (false, s) => s
    }
  }

  // Enumerates the reflexive transitive closure ->* and shows a trace of
  // rewrite steps for each term
  def rewriteTrace(rs: List[Rule],t: Term): List[Trace] =
  rewriteTraceRec(rs, Set(Trace(List(t)))).toList

  // Eagerly applies rewriting until obtaining an irreducible term.
  // This function is not guaranteed to terminate in the presence of
  // an infinitely descending chains even if there is a reachable
  // irreducible term.
  def normalise(rs: List[Rule],t: Term): Term = rewrite(rs,t) match{
    case Nil => t
    case rw :: _ => normalise(rs,rw)
  }

  def normaliseTraceRec(rs: List[Rule],ts: List[Term]): List[Term] =
    rewrite(rs, ts.head) match{
      case Nil => ts
      case rw :: _ => normaliseTraceRec(rs, rw :: ts)
    }

  // Returns a trace of the rewrite steps of normalization
  def normaliseTrace(rs: List[Rule],t: Term) = Trace(normaliseTraceRec(rs,List(t)))
}
