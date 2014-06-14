package looneesha

import scala.language.dynamics
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.collection.immutable.HashMap

case class GraphBuilder(mapping: Map[String, List[AtomDF] => List[Double]],
                        prefix: String,
                        separator: String = "_") {

  var cfs: List[CF] = Nil
  def addprefix(name: String) = prefix + separator + name

  object defn extends Dynamic {
    def applyDynamic(m: String)(args: => (List[DF], List[DF])) = {
      val (in, out) = args
      (in ::: out) forall (_.isInstanceOf[AtomDF]) match {
        case true => cfs ::= AtomCF(addprefix(m), mapping(m), in.asInstanceOf[List[AtomDF]], out.asInstanceOf[List[AtomDF]])
        case false => cfs ::= MetaCF(addprefix(m), mapping(m), in, out)
      }
    }
  }

  class io extends Dynamic {
    def applyDynamic(f: String)(args: DF*) = args.toList
  }

  object in extends io
  object out extends io

  object % extends Dynamic {
    def fixname(name: String) = name contains separator match {
      case true => name
      case false => addprefix(name)
    }

    def selectDynamic(name: String) = AtomDF(fixname(name))
    def applyDynamic(name: String)(ind: MetaIndex) = MetaDF(fixname(name), ind.i)
    //def applyDynamic(name: String)(ind: Int) = AtomDF(fixname(name), ind)
  }

  val * = MetaIndex(0)

  case class MetaIndex(i: Int) {
    def + (x: Int) = MetaIndex(x + i)
    def - (x: Int) = MetaIndex(x - i)
  }

  def get = Graph(cfs)

  def + (g: GraphBuilder) = {
    val r = new GraphBuilder(new HashMap[String, List[AtomDF] => List[Double]], "no")
    r.cfs = g.cfs ::: this.cfs
    r
  }
}
