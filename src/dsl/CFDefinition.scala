package looneesha

trait CFDefinition {
  def mapping: Map[String, List[AtomDF] => List[Double]]
  def apply = mapping

  implicit def AtomDFtoValue(df: AtomDF) = df.value
}
