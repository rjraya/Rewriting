package rewriter

//TODO: adapt to cake pattern
object Confluence {

  import rewriter.Rewriter._

  //TODO: see if we can avoid passing over parameters
  def makeVarRenaming(s: Set[String],l: List[String]): Map[String,String] = {
    def renameVar(s: Set[String], m: Map[String, String], x: String): Map[String, String] =
      if (s contains x) m + (x -> findFreeName(s, x, 2))
      else m

    def findFreeName(s: Set[String], x: String, n: Int): String =
      if (s contains (x ++ n.toString())) findFreeName(s, x, n + 1)
      else x ++ n.toString()

    l.foldLeft(Map.empty: Map[String,String])(
      (acc: Map[String,String],st: String) => renameVar(s, acc, st))
  }

  def makeVarsDisjoint(r1: Rule,r2: Rule): (Rule,Rule) = {
    def renameRuleVars(f: String => String)(r: Rule) =
      Rule(renameVars(f,r.left),renameVars(f,r.right))
    val v1 = variablesHashSet(r1.left) | variablesHashSet(r1.right)
    val v2 = variablesHashSet(r2.left) | variablesHashSet(r2.right)
    val m = makeVarRenaming(v1,v2.toList)
    val renameRuleVarsRec = renameRuleVars(x => m.getOrElse(x,x))

    (r1,renameRuleVarsRec(r2))
  }

  //TODO: check whether the unifyOption match is well-done
  def criticalPairs2(l1: Term,r1: Term,l2: Term,r2: Term,b: Boolean): List[(Subst,Term)] =
    (l1, r1, l2, r2, b) match {
      case (Var(_), _, _, _, _) | (_, _, Var(_), _, _) => Nil
      case (s@Fun(f, args),_,_,_,noRoot) =>
        val critPair = unifyOption(s, l2) match {
          case None => Nil
          case Some(cs) => List((cs, r2))
        }

        def childrenCrits(largs: List[Term], rargs: List[Term]): List[(Subst, Term)] = (largs, rargs) match {
          case (_, Nil) => Nil
          case (clargs, x :: crargs) =>
            (for ((s, fr2) <- criticalPairs2(x, r1, l2, r2, false: Boolean))
              yield (s, Fun(f, clargs ++ List[Term](fr2) ++ crargs))) ++ childrenCrits(Nil, args)
        }

        (if (noRoot) Nil else critPair) ++ childrenCrits(Nil, args)
    }

  def criticalPairs1(l1: Term,r1: Term,l2: Term,r2: Term,b: Boolean): List[(Term,Term,Term)] = {
    def f(p: (Subst,Term)): (Term, Term, Term) = p match{
      case (s,fr2) => (applySubst(s, l1), applySubst(s, r1), applySubst(s, fr2))
    }

    criticalPairs2(l1, r1, l2, r2, b) map f
  }

  def criticalPairs(l: List[Rule]): List[(Term,Term,Term)] = {
    def criticalPairsl2(r1: Rule, r2: Rule,b: Boolean): List[(Term,Term,Term)] = {
      criticalPairs1(r1.left, r1.right, r2.left, r2.right, b)
    }
    def criticalPairsWithR(r: Rule,dr: Rule): List[(Term,Term,Term)] = {
      criticalPairsl2(r, dr, false: Boolean) ++ criticalPairsl2(dr, r, false: Boolean)
    }

    l match {
      case Nil => Nil
      case r :: rs =>
        criticalPairsl2(r, r, true: Boolean) ++ (rs flatMap{ criticalPairsWithR(r,_) }) ++ criticalPairs(rs)
    }
  }

  def isWeaklyConfluent(rs: List[Rule]): Boolean = {
    def equivalent(p: (Term,Term,Term)): Boolean = p match{
      case (o,s,t) => normalise(rs, s) == normalise(rs, t)
    }
    criticalPairs(rs).forall{ equivalent }
  }
}
