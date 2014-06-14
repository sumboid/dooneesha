package looneesha

import scala.language.dynamics
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.collection.immutable.HashMap

case class ProblemBuilder {
  var dfs: List[AtomDF] = Nil
  var question: List[AtomDF] = Nil

  object set {
    def apply(y: => (AtomDF, Double)) = { dfs ::= y._1.set(y._2) }
  }

  object ? {
    def apply(dfs: AtomDF*) = { dfs foreach (question ::= _) }
  }

  object % extends Dynamic {
    def selectDynamic(name: String) = AtomDF(name)
    def applyDynamic(name: String)(ind: Int) = AtomDF(name, ind)
  }
}
