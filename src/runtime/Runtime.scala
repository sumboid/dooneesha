package looneesha

import akka.actor.Actor
import akka.actor.Props
import akka.actor.ActorSystem

class Worker extends Actor {
  def receive = {
    case cf: AtomCF => cf.run foreach (sender ! _)
  }
}

class Runtime(graph: Graph, needed: List[AtomDF]) extends Actor {
  var cfs: List[(AtomCF, Boolean)] = Nil
  var kdfs: List[AtomDF] = Nil
  val worker = context.actorOf(Props[Worker], name = "worker")

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

  def receive = {
    case df: AtomDF => {
      println("Look! " + df + " = " + df.value)
      kdfs ::= df
      tryCompute
    }
  }

  override def preStart = {
    println("Initializing")
    graph.subgraph(needed) foreach (cf => cfs ::= cf -> false)
    tryCompute
  }
}

object Runtime {
  def apply(g: GraphBuilder, n: List[AtomDF]) = {
    val context = ActorSystem("Dooneesha")
    context.actorOf(Props(classOf[Runtime], g.get, n), name = "runtime")
  }
}