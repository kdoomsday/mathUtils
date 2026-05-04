import org.scalacheck.Properties
import file.LoadObject
object TestAstar extends Properties("Astar") {
  import graph._

  /* For testing we will use a matrix as the graph. 0 Means can't be reached
     Any other number is cost to get from adjacent here */
  case class Node(x: Int, y: Int) {
    def adjacents = List(Node(x-1, y), Node(x+1, y), Node(x, y-1), Node(x, y+1))
  }


  property("mAstar works simple case") = {
    val simpleM = List(List(1, 1, 2),
                       List(0, 1, 2),
                       List(0, 1, 1))
    val h = simpleM.size
    val w = simpleM(0).size

    val start = Node(0, 0)
    val goal = Node(w-1, h-1)
    def neighbors(n: Node) =
        n.adjacents.filter(np => np.x >= 0 && np.x < w && np.y >= 0 && np.y < h)
    val heuristic: (Node, Node) => Float = (n1, n2) => 0.0f
    def cost: (Node, Node) => Float = (n1, n2) => simpleM(n2.y)(n2.x).toFloat

    val astarRes  = Graph.astar(start, goal, neighbors, heuristic, cost)
    val mAstarRes = Graph.mAstar(start, goal, neighbors, heuristic, cost)

    astarRes == mAstarRes
  }

  property("astar on a bigger matrix") = {
    val begin = System.nanoTime()
    val m = LoadObject.loadMatrixResource("astarMatrix.txt", " ")
    val h = m.length
    val w = m(0).length
    val start = Node(0, 0)
    val goal = Node(w-1, h-1)
    def neighbors(n: Node) =
      n.adjacents.filter(np => np.x >= 0 && np.x < w && np.y >= 0 && np.y < h
                           && m(np.y)(np.x) > 0)
    val heuristic: (Node, Node) => Float = (n1, n2) => 0.0f
    def cost: (Node, Node) => Float = (n1, n2) => m(n2.y)(n2.x).toFloat

    val astarRes = Graph.astar(start, goal, neighbors, heuristic, cost)
    astarRes.nonEmpty
  }

  property("mAstar on a bigger matrix") = {
    val begin = System.nanoTime()
    val m = LoadObject.loadMatrixResource("astarMatrix.txt", " ")
    val h = m.length
    val w = m(0).length
    val start = Node(0, 0)
    val goal = Node(w-1, h-1)
    def neighbors(n: Node) =
      n.adjacents.filter(np => np.x >= 0 && np.x < w && np.y >= 0 && np.y < h
                           && m(np.y)(np.x) > 0)
    val heuristic: (Node, Node) => Float = (n1, n2) => 0.0f
    def cost: (Node, Node) => Float = (n1, n2) => m(n2.y)(n2.x).toFloat

    val mAstarRes = Graph.mAstar(start, goal, neighbors, heuristic, cost)
    mAstarRes.nonEmpty
  }

  property("aStar on unsolvable graph") = {
    val m = LoadObject.loadMatrixResource("unSolveAstarMatrix.txt", " ")
    val h = m.length
    val w = m(0).length
    val start = Node(0, 0)
    val goal = Node(w-1, h-1)
    def neighbors(n: Node) =
      n.adjacents.filter(np => np.x >= 0 && np.x < w && np.y >= 0 && np.y < h
                           && m(np.y)(np.x) > 0)
    val heuristic: (Node, Node) => Float = (n1, n2) => 0.0f
    def cost: (Node, Node) => Float = (n1, n2) => m(n2.y)(n2.x).toFloat

    val astarRes = Graph.astar(start, goal, neighbors, heuristic, cost)
    astarRes.isEmpty
  }
}
