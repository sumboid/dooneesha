package looneesha

trait DF extends Fragment {
  def name:String
  override def equals(other: Any) = other match {
    case x: DF => this.name == x.name
    case _ => false
  }
}

case class AtomDF(name: String, index: Int = 0, value: Double = 0, defined: Boolean = false) extends DF {
  override def equals (other: Any) = other match {
    case x: AtomDF => this.name == x.name && this.index == x.index
    case _ => false
  }

  def set (v: Double) = AtomDF(name, index, v, true)
  override def toString = name + "(" + index + ")"
}

case class MetaDF(name: String, metaindex: Int) extends DF {
  def createAtomDF(ind: Int) = ind + metaindex >= 0 match {
    case true => Some(AtomDF(name, ind + metaindex))
    case false => None
  }
}
