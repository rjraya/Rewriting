package rewriter

import rewriter.Parser.{parseTRSFile,ParseResult,Success,Failure,consoleTerm,consoleTerms,parseAll,checkTerm}
import rewriter.Printer.{prettyPrintRule,prettyPrintSubst,prettyPrint,prettyPrintTrace}
import rewriter.TRS.TRS
import rewriter.Rewriter.{Term,applySubst,unify,mmatch,rewrite,rewriteTrace,normalise,normaliseTrace}
import rewriter.Confluence.{criticalPairs,isWeaklyConfluent}
import scala.io.StdIn.readLine

object Console {

  val commands = List("help", "rules", "term", "unify", "match", "rewrite", "trace", "normalise",
    "normalise_trace", "critical_pairs", "locally_confluent", "quit")
  val helpMessage = "Available commands:" ++ commands.flatMap("\n" ++ _)
  //TODO: see if this is the right structure for the list
  val commandHelpMessages = List(
    "help <command>\n"++
      "    Without an argument: displays a list of all available commands.\n" ++
      "    With a command as an argument: displays a description of the given \n" ++
      "    command.",
    "rules\n"++
      "    Prints a list of all the rules in the term rewriting system.\n",
    "term <term>\n"++
      "    Parses the given term and displays its internal (non-pretty-printed) \n" ++
      "    representation.\n",
    "unify <term1> and <term2>\n" ++
      "    Prints the most general unifier σ for the given two terms t1 and t2, \n" ++
      "    if a unifier exists. The term σ(t1) = σ(t2) is also printed.",
    "match <term1> to <term2>\n" ++
      "    Prints the matcher σ for the first given term t1 to the second one t2, \n" ++
      "    if it exists. A matcher is a substitution such that σ(t1) = t2.",
    "rewrite [<term>]\n"++
      "    Applies a single rewrite step to the given term and displays a list of\n"++
      "    all possible results. (There may be several, as several rules may be\n"++
      "    applicable, and the same rule may be applicable in different positions\n"++
      "    of the term.)\n"++
      "    If no term is given, the first term in the last result list is used.\n",
    "trace [<term>]\n"++
      "    Iteratively applies the rewrite rules until a normal form is reached and\n"++
      "    then displays a rewriting trace for every distinct normal form.\n"++
      "    If no term is given, the first term in the last result list is used.\n",
    "normalise [<term>]\n"++
      "    Eagerly applies all rewrite rules iteratively until a normal form is used\n"++
      "    and then returns this normal form.\n"++
      "    If no term is given, the first term in the last result list is used.\n",
    "normalise_trace [<term>]\n"++
      "    Eagerly applies all rewrite rules iteratively until a normal form is used\n"++
      "    and then returns a trace from the original term to this normal form.\n"++
      "    If no term is given, the first term in the last result list is used.\n",
    "critical_pairs\n"++
      "    Prints a list of all critical pairs of the term rewriting system.\n",
    "locally_confluent [verbose]\n"++
      "    Tries to determine whether the term rewriting system is locally \n"++
      "    confluent, i.e. whether any split of the form t1 ← s → t2 can be\n" ++
      "    rejoined with t1 →* t ←* t2.\n" ++
      "    If the option \"verbose\" is given, a list of all critical pairs and \n" ++
      "    whether they are joinable or not is also printed.\n",
    "quit\n"++
      "    Ends the programme.\n")

  def doHelp(s: String): Unit = {
    val i = commands.indexOf(s)
    if (i == -1) println("No such command.") else println("\n" ++ commandHelpMessages(i))
  }

  //TODO: check ops should be trs?
  def doShowRules(trs: TRS): Unit = trs match {
    case TRS.TRS(ops,rs) =>  println(rs.map(prettyPrintRule(ops.values.toList,_)).mkString("\n"))
  }

  def doShowTerm(o: Option[Term]): Option[Term] = o match{
    case Some(t) => println(t); Some(t)
    case None => None
  }

  def doUnify(trs: TRS,o: Option[(Option[Term],Option[Term])]): Option[Term] = o match{
    case None => println("Invalid parameters. Please type " ++ "\"unify <term1> and <term2>\"."); None
    case Some((None,_)) => println("First term provided is invalid"); None
    case Some((_,None)) => println("Second term provided is invalid"); None
    case Some((Some(t1),Some(t2))) => unify(t1,t2) match{
      case Left(e) => println("No unifiers: " ++ e.print); None
      case Right(s) =>
        val ct = applySubst(s,t1)
        //TODO: check printing with ops instead of trs
        println("Most general unifier:\n" ++ prettyPrintSubst(trs.ops.values.toList,s) ++ "\n\n" ++
          "Corresponding term: " ++ prettyPrint(trs.ops.values.toList,ct))
        Some(ct)
    }
  }

  //TODO: there was no return type just Unit
  def doMatch(trs: TRS,o: Option[(Option[Term],Option[Term])]): Unit = o match{
    case None => println("Invalid parameters. Please type " ++ "\"match <term1> to <term2>\".")
    case Some((None,_)) => println("First term provided is invalid")
    case Some((_,None)) => println("Second term provided is invalid")
    case Some((Some(t1),Some(t2))) => mmatch(t1,t2) match{
      case Left(e) => println("No matchers: " ++ e.print)
      case Right(s) => println("Matcher:\n" ++ prettyPrintSubst(trs.ops.values.toList,s))
    }
  }

  def doRewrite(trs: TRS,o: Option[Term]): Option[Term] = o match{
    case None => println("Term provided is invalid"); None
    case Some(t) =>
      val ts = rewrite(trs.rs,t)
      ts match{
        case Nil => println("Term is already in normal form."); None
        case r :: _ =>
          println((List.range(1,ts.length) ,ts.map(prettyPrint(trs.ops.values.toList,_))).zipped.map((a,b) => a.toString ++ ": " ++ b).mkString("\n"))
          Some(r)
      }
  }

  def doTrace(trs: TRS,o: Option[Term]): Option[Term] = o match{
    case None => println("Term provided is invalid"); None
    case Some(t) =>
      val traces = rewriteTrace(trs.rs,t)
      traces match{
        case Nil => println("No result."); None
        case _ => println((List.range(1,traces.length),traces.map(prettyPrintTrace(trs.ops.values.toList,_))).zipped.map(
          (a,b) => "Trace " ++ a.toString ++ ":\n" ++ b).mkString("\n\n"))
          None
      }
  }

  def doNormalise(trs: TRS,o: Option[Term]): Option[Term] = o match{
    case None => println("Term provided is invalid"); None
    case Some(t) => println(prettyPrint(trs.ops.values.toList,normalise(trs.rs,t))); None
  }

  def doNormaliseTrace(trs: TRS,o: Option[Term]): Option[Term] = o match{
    case None => println("Term provided is invalid"); None
    case Some(t) => println(prettyPrintTrace(trs.ops.values.toList,normaliseTrace(trs.rs,t))); None
  }

  def doCriticalPairs(trs: TRS): Unit = {
    def pretty(t: Term) = prettyPrint(trs.ops.values.toList,t)
    def formatPair(i: Integer,trip: (Term,Term,Term)): String = trip match { case (s,t1,t2) =>
      "Critical pair " ++ i.toString ++ ":\n" ++
      pretty(s) ++ " → " ++ pretty(t1) ++ "\n" ++
      pretty(s) ++ " → " ++ pretty(t2) ++ "\n\n"
    }

    val cps = criticalPairs(trs.rs)
    cps match{
      case Nil => println("No critical pairs.")
      case _ =>
        print((List.range(1,cps.length) zip cps).flatMap{ case (i,(s,t1,t2)) =>
          formatPair(i,(s,t1,t2))
        })
    }
  }

  def doLocallyConfluent(trs: TRS,verbose: Boolean): Unit = {
    def pretty(t: Term) = prettyPrint(trs.ops.values.toList, t)

    def formatPair(i: Integer, quint: (Term,Term,Term,Term,Term)) = quint match { case (s, t1, t2, n1, n2) =>
      "Unjoinable critical pair " ++ i.toString ++ ":\n" ++
      pretty(s) ++ " → " ++ pretty(t1) ++ " →* " ++ pretty(n1) ++ " (irreducible)\n" ++
      pretty(s) ++ " → " ++ pretty(t2) ++ " →* " ++ pretty(n2) ++  " (irreducible)\n\n"
    }

    val cps = {
      criticalPairs(trs.rs).flatMap { case (s, t1, t2) =>
        val n1 = normalise(trs.rs,t1)
        val n2 = normalise(trs.rs,t2)
        if(n1 == n2) Nil else List((s,t1,t2,n1,n2))
      }
    }

    print(
      (if (verbose) (List.range(1,cps.length) zip cps).flatMap({ case (i,(s,t1,t2,n1,n2)) =>
        formatPair(i,(s,t1,t2,n1,n2))
      }) else Nil) ++
      ("System is " ++ (if (isWeaklyConfluent(trs.rs)) Nil else "not ") ++
        "locally confluent.\n")
    )
  }

  def extractTerm(trs: TRS,s: String): Option[Term] = parseAll(consoleTerm,s) match{
    case Success(t,_) =>
      if(checkTerm(trs,t)) Some(t) else None
    case Failure(msg,_) => println(msg); None
  }

  // Case to manage: unify t1 and t2, match t1 to t2
  def extractTerms(trs: TRS,s: String,sep: String): Option[(Option[Term],Option[Term])] = parseAll(consoleTerms,s) match{
    case Success((t1,t2),_) =>
      if(checkTerm(trs,t1) && checkTerm(trs,t2)) Some(Some(t1),Some(t2)) else None
    case Failure(msg,_) => println(msg); None
  }


  def commandLoop(trs: TRS,last: Option[Term]): Unit = {
    val str = readLine("TRS> ", Nil)
    str.toLowerCase().split(" ").toList match {
      // Generic cases
      case Nil => commandLoop(trs, last)
      case List("quit",_*) => ()
      case List("help") => println(helpMessage); commandLoop(trs, last)
      case List("help",cmd: String,_*) => doHelp(cmd); commandLoop(trs, last)
      case List("rules",_*) => doShowRules(trs); commandLoop(trs, last)
      // Cases that use a previously parsed term
      case List("term") => last match {
        case None => println("No current result. Type 'term <term>'"); commandLoop(trs, None)
        case Some(t) => commandLoop(trs, doShowTerm(Some(t)))
      }
      case List("rewrite") => last match {
        case None => println("No current result. Type 'rewrite <term>'"); commandLoop(trs, None)
        case Some(t) => commandLoop(trs, doRewrite(trs, Some(t)))
      }
      case List("trace") => last match {
        case None => println("No current result. Type 'trace <term>'"); commandLoop(trs, None)
        case Some(t) => commandLoop(trs, doTrace(trs, Some(t)))
      }
      case List("normalise") => last match {
        case None => println("No current result. Type 'normalise <term>'"); commandLoop(trs, None)
        case Some(t) => commandLoop(trs, doNormalise(trs, Some(t)))
      }
      case List("normalise_trace") => last match {
        case None => println("No current result. Type 'normalise_trace <term>'"); commandLoop(trs, None)
        case Some(t) => commandLoop(trs, doNormaliseTrace(trs, Some(t)))
      }
      // Cases that need to parse a term
      case List("term",_*) => commandLoop(trs, doShowTerm(extractTerm(trs, str)))
      case List("unify",_*) => commandLoop(trs, doUnify(trs, extractTerms(trs, str, "and")))
      case List("match",_*) => doMatch(trs,extractTerms(trs,str,"to")); commandLoop(trs,last)
      case List("rewrite",_*) => commandLoop(trs,doRewrite(trs,extractTerm(trs,str)))
      case List("trace",_*) => commandLoop(trs,doTrace(trs,extractTerm(trs,str)))
      case List("normalise",_*) => commandLoop(trs,doNormalise(trs,extractTerm(trs,str)))
      case List("normalise_trace",_*) => commandLoop(trs,doNormaliseTrace(trs,extractTerm(trs,str)))
      case List("critical_pairs",_*) => doCriticalPairs(trs); commandLoop(trs,None)
      case List("locally_confluent","verbose",_*) => doLocallyConfluent(trs,true); commandLoop(trs,None)
      case List("locally_confluent",_*) => doLocallyConfluent(trs,false); commandLoop(trs,None)
      case xs@_ => println("Unknown command: " ++ xs); commandLoop(trs,last)
    }
  }

 def main(args: Array[String]): Unit = {
   if (args.length < 1) {
     println("Usage: rewrite <TRS file>")
   } else {
     val parseResult = parseTRSFile(args(0))
     if (parseResult._1.isEmpty && parseResult._2.isEmpty) {
       println("Parsing did not provide rewrite rules or function symbols")
     } else {
       val trs = new TRS(parseResult._1, parseResult._2)
       commandLoop(trs, None)
     }
   }
 }  
}
