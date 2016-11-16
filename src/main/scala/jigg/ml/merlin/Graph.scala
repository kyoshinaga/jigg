package jigg.ml.merlin

import jigg.util.hdfutil._

import scala.language.postfixOps

/**
  * Created by ubuntu on 10/17/16.
  */
object Graph {

  def constructGraph(model: H5Node) : List[Functor] = {
    if (!model.hasChild)
      throw new IllegalAccessError("cannot construct graph with empty node")
    else if (!model.child.exists(_.label == "Merlin"))
      throw new IllegalAccessError("cannot construct graph with the root node not including Merlin node")

    val merlinNode = model.child.filter(_.label == "Merlin").head

    if (!merlinNode.child.exists(_.label == "model"))
      throw new IllegalAccessError("cannot construct graph because this node has no `model` node")

    val modelNode = merlinNode.child.filter(_.label == "model").head.child.filterNot(_.label == "#TYPE")

    if (modelNode.length < 1)
      throw new IllegalAccessError("cannot construct graph with empty model")

    val functors = {
      for {
        i <- 1 to modelNode.length
        n = modelNode.filter(_.label == i.toString).head.child.head
        f = getFunctor(n)
      } yield f
    } collect{case x:Some[Functor] => x.get} toList

    functors
  }

  private def getFunctor(n: H5Node): Option[Functor] = n match {
    case x:H5Node if n.child.head.data.head.toString == "Function" =>
      x.child(1).data.head.toString match{
        case "Merlin.relu" => Some(Relu)
        case "transpose" => Some(Transpose)
        case _ => None
      }
    case x:H5Node => x.child.head.data.head.toString match {
      case "Merlin.Embedding" => Some(Embedding(x))
      case "Merlin.Conv{2}" => Some(Conv(x))
      case "Merlin.Linear" => Some(Linear(x))
      case _ => None
    }
  }

}
