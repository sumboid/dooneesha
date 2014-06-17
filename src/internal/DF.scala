package looneesha

trait DF extends Fragment {
  def name:String
  override def equals(other: Any) = other match {
    case x: DF => this.name == x.name
    case _ => false
  }
}

case class AtomDF(name: String, index: Integer = 0, value: Double = 0, defined: Boolean = false) extends DF {
  override def equals (other: Any) = other match {
    case x: AtomDF => this.name == x.name && this.index == x.index
    case _ => false
  }

  def set (v: Double) = AtomDF(name, index, v, true)
  override def toString = name + "(" + index + ")"
}

case class MigratedDF(df: AtomDF) {
  override def equals(other: Any) = other match {
    case x: MigratedDF => this.df == x.df
    case _ => false
  }
}

case class MetaDF(name: String, metaindex: Int) extends DF {
  def createAtomDF(ind: Int) = AtomDF(name, ind + metaindex)
}
