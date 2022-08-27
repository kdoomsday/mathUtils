package math

import General.{ factorial, pow }

object Counting {

  // n choose k
  def comb(n: BigInt, k: BigInt): BigInt = factorial(n) / (factorial(k) * factorial(n-k))


  /** *** DICE ***
    * Number of ways to roll exactly a sum given:
    * n -> Number of dice
    * sum -> Sum I want to achieve
    * sides -> Number of sides in each dice
    *
    * We asume all dice are numbered 1 through 'sides'
    */
  def coefficient(n: Int, sum: Int, sides: Int): BigInt = {
    (0 to (sum-n)/sides).map(k => pow(-1, k) * comb(n, k) * comb((sum - sides*k -1), n-1)).sum
  }
}
