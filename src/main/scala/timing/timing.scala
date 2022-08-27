package timing

/** Helpers to time executions */
object Timing {

  /**
    * Execute a code block, timing it
    *
    * @param f by-name code block
    * @return Return value of the block with how many nanos it took to execute
    */
  def timeRes[A](f: => A): (A, Long) = {
    val begin = System.nanoTime()
    val res = f
    (res, System.nanoTime() - begin)
  }

  /**
   * Execute a code block, time it and print both the result and how much time
   * it took
   */
  def runAndGun[A](block: =>A): Unit = {
    val (res, time) = timeRes(block)
    printf("Answer is: %s (%d ms)%n", res, time)
  }
}
