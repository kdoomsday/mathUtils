package file

object LoadObject {
  /**
   * Reads a line and returns the numbers in it
   *
   * @param line The line
   * @param sep Separator expected between the numbers
   * @return Array of numbers in the line
   */
  private def numerizer(line: String, sep: String): Array[Int] = line.split(sep).filterNot(_ == "").map(_.toInt)

  /** Load an Int matrix from a file */
  def loadMatrix(filename: String, sep: String = ","): Array[Array[Int]] = {
    val lines = scala.io.Source.fromFile(filename).getLines()
    lines.toArray.map(line => numerizer(line, sep))
  }

  /** Load an Int matrix from a file */
  def loadMatrixResource(resource: String, sep: String = ","): Array[Array[Int]] = {
    val cl = Thread.currentThread().getContextClassLoader()
    val lines = scala.io.Source.fromInputStream(cl.getResourceAsStream(resource)).getLines()
    lines.toArray.map(line => numerizer(line, sep))
  }
}
