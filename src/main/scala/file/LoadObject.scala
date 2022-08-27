package file

object LoadObject {
  /** Load an Int matrix from a file */
  def loadMatrix(filename: String, sep: String = ","): Array[Array[Int]] = {
    def numerizer(line: String): Array[Int] = line.split(sep).filterNot(_ == "").map(_.toInt)

    val lines = scala.io.Source.fromFile(filename).getLines

    lines.toArray.map(numerizer)
  }
}
