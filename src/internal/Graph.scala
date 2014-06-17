package looneesha

case class Graph(cfs: List[CF]) {
  def subgraph(needed: List[AtomDF]) = {
    var result: List[AtomCF] = Nil
    needed
      .foreach (df => cfs
        .filter (cf => cf.out contains df)
        .find (cf => { println("I've found" + cf); cf match {
          case x: AtomCF => result ::= x; true
          case x: MetaCF => val c = x.createAtomCF(df); if (c != None) { result ::= c.get; true } else false
         }}))
    result
  }
}
