package graph

object Graph {
  /**
   * A* path search algorithm. Works on paths of any type.
   * start: Start node
   * goal: End node
   * neighbors: Function that returns every neighbor of a node
   * heuristic: Function that estimates the cost of going from one node to another. Has to be
   *            admissible (no overestimation) and consistent
   * costFunc:  Function to get the cost of moving from one node to another. It is guaranteed to
   *            only be called on neighbors
   *
   * Returns the list of nodes to traverse.
   */
  def astar[T](start: T, goal: T, neighbors: T => List[T], heuristic: (T,T)=>Float, costFunc: (T,T) => Float): List[T] = {
    def calculateCost(path:(Float, List[T])) =
      path._2.tail.foldLeft((0f,path._2.head))((costHead,node) => {
          val (cost, head) = costHead
          (cost + costFunc(node,head), node)
        })._1

    def findBestCandidate(candidates:Set[(Float, List[T])]) =
      candidates.foldLeft(
        (Float.PositiveInfinity, (0f, List[T]())))((best, path) => {
          val guesstimate = path._1 + heuristic(path._2.head, goal)
          if (best._1 <  guesstimate) best else (guesstimate,path)
        })._2

    // Uses mutable set for performance... ?
    var fringe = Set((0f,start::Nil))
    var closed = Set[T]()

    while(!fringe.isEmpty) {
      val (cost, current) = findBestCandidate(fringe)
      fringe = fringe - ((cost, current))
      if(current.head equals goal)
        return current.reverse
      else if (!closed.contains(current.head )) {
        closed = closed + current.head
        neighbors(current.head).foreach(n => {
            val nCost = costFunc(current.head, n) + cost
            fringe = fringe + ((nCost, n::current))
          })
      }
    }
    // Searched all possible paths...
    return Nil
  }
}
