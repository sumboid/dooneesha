package looneesha

import scala.actors.Actor 

class Runtime(graph: Graph, input: List[AtomDF], output: List[AtomDF]) extends Actor {
  var actors: List[Actor] = Nil
  var out = output.toArray
  var cannotExec = false

  def visualize = GV.create(graph, Graph(graph.paths(input, output).flatMap(_.cfs)), input, output).draw
  def writeData = {
    println("Graph: " + graph)
    println("Input: " + input)
    println("Output: " + output)
  }

  def writeSolution = println("Paths: " + graph.paths(input, output))

  def defineDF(df: AtomDF) = {
    val index = out indexOf df
    out update (index, df)
  }

  def end = out forall (_.defined)

  def act = {
    if (cannotExec) exit
    loop {
      react {
        case df: AtomDF => {
          println("Look! " + df + " = " + df.value)
          defineDF(df)
          if(end) exit
        }
      }
    }
  }

  def init = {
    if (output exists (graph.paths(input, _) == Nil)) { println ("No path"); cannotExec = true }
      else {
      output foreach (out => initComputation(graph.paths(input, out)(0), out))
      actors foreach (_.start)
      input foreach (in => actors filter (_.asInstanceOf[AtomCFRuntime].cf.in contains in) foreach (_ ! in)) 
    }
  }

  def initComputation(g: Graph, out: AtomDF) = {

    def getlink(dfs: List[AtomDF]) = g.specialFilterIn(dfs) match {
      case Nil => dfs map (df => df -> this)
      case cfs => dfs flatMap (df => actors filter (a => a.asInstanceOf[AtomCFRuntime].cf.in contains df) map (a => df -> a))
    }

    def _initComputation(cfs: List[AtomCF]): Unit = cfs match {
      case Nil => ()
      case _   => {
        cfs foreach (cf => actors ::= AtomCFRuntime(cf, getlink(cf.out)))
        _initComputation(g.filterOut(cfs flatMap (_.in)).asInstanceOf[List[AtomCF]])
      }
    } 

    _initComputation(g.filterOut(out :: Nil).asInstanceOf[List[AtomCF]])
  }
}

object Runtime {
  def apply(g: GraphBuilder, p: ProblemBuilder) = new Runtime(g.get, p.dfs, p.question)
}

case class AtomCFRuntime(cf: AtomCF, link: List[(AtomDF, Actor)]) extends Actor {
  private[this] var input = cf.in.toArray

  def defineDF(df: AtomDF) = {
    val index = input indexOf df
    input update (index, df)
  }

  def tryRun = if (input forall (_.defined)) {
    val realcf = cf.set(input.toList)
    val result = realcf.run
    result foreach (df => link filter (l => l._1 == df) foreach (l => l._2 ! df))
    exit
  }

  def defineAndTryRun(df: AtomDF) = {
    defineDF(df)
    tryRun
  }

  def act = loop {
    react {
      case df: AtomDF => defineAndTryRun(df)
    }
  }
}
