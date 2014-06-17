package looneesha

import scala.actors.Actor

class Worker(system: Runtime) extends Actor {
  def act = {
    loop {
      react {
        case cf: AtomCF => cf.run foreach (system ! _)
      }
    }
  }
}

class Runtime(graph: Graph, needed: List[AtomDF]) extends Actor {
  var cfs: List[(AtomCF, Boolean)] = Nil
  var kdfs: List[AtomDF] = Nil
  val worker = new Worker(this)
  worker.start

  def tryCompute = {
    var acfs = cfs.toArray
    cfs filter (_._2 == false) foreach (cf => {
      val dfs = cf._1.in
      if(dfs == Nil) {
        val i = acfs.indexOf(cf)
        acfs.update(i, cf._1 -> true)
        val rcf = cf._1
        worker ! rcf
      } else {
        var fdfs: List[AtomDF] = Nil
        dfs forall (df => kdfs.find(_ == df) match {
          case None => false
          case Some(x) => fdfs ::= x; true
        }) match {
          case false => ()
          case true => {
            val i = acfs.indexOf(cf)
            acfs.update(i, cf._1 -> true)
            val rcf = cf._1.set(fdfs)
            worker ! rcf
          }
        }
    }})
    cfs = acfs.toList
  }

  def act = {
    loop {
      react {
        case df: AtomDF => {
          println("Look! " + df + " = " + df.value)
          kdfs ::= df
          tryCompute
        }
      }
    }
  }

  def init = {
    println("Initializing")
    graph.subgraph(needed) foreach (cf => cfs ::= cf -> false)
    tryCompute
  }
}

object Runtime {
  def apply(g: GraphBuilder, n: List[AtomDF]) = new Runtime(g.get, n)
}