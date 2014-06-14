package looneesha
import sys.process._

case class GV(highlight: List[(Fragment, String)],
              links: List[(Fragment, Fragment)],
              opts: List[(Fragment, String)]) {
  def generate = {
    (List("digraph G {") :::
      opts.map(x => x._1.name + " " + x._2 + ";") :::
      highlight.map(x => x._1.name + " " + x._2 + ";") :::
      links.map(x => x._1.name + " -> " + x._2.name + ";") :::
      List("}")).mkString
  }

  def draw = (("echo " + generate) #| "dot -o1.svg -Tsvg" #&& "eog 1.svg" #&& "rm 1.svg").!
}

object GV {
  def create(g: Graph) = {
    val links = g.cfs.flatMap(cf => cf.in.map(_ -> cf) ::: cf.out.map(cf ->))
    val opts = g.cfs.map(_ -> "[shape=box]")
    GV(Nil, links, opts)
  }
  def create(g: Graph, sg: Graph, in: List[Fragment], out: List[Fragment]) = {
    val links = g.cfs.flatMap(cf => cf.in.map(_ -> cf) ::: cf.out.map(cf ->))
    val opts = g.cfs.map(_ -> "[shape=box]")
    val highlightCF = sg.cfs.map(_ -> "[color=red]")
    val highlightDF = sg.cfs.flatMap(cf => cf.in.map(_ -> "[color=red]") ::: cf.out.map(_ -> "[color=red]"))
    val highlight = highlightCF ::: highlightDF
    GV(highlight, links, opts)
  }
}
