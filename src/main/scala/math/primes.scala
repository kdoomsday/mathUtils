package math

import scala.math.sqrt

/**
 * Funciones relacionadas con numeros primos. Calcular primos, determinar si un
 * numero es primo, etc.
 */
object Primes {
  /**
   * Utiliza el Sieve of Erathosthenes para generar todos los primos hasta max
   */
  def primeSieve(max: Int): Seq[Int] = {
    val isPrimeArr = Array.fill(max+1)(true)
    isPrimeArr(0) = false
    isPrimeArr(1) = false

    (2 until isPrimeArr.size).foreach(i =>
      if (isPrimeArr(i)) {
        var j = i+i
        while (j < isPrimeArr.size) {
          isPrimeArr(j) = false
          j += i
        }
      }
    )

    for (i <- 2 until isPrimeArr.size; if (isPrimeArr(i))) yield i
  }

  /** Sieve que genera Set en lugar de Seq */
  def setSieve(max: Int): Set[Int] = {
    val isPrimeArr = Array.fill(max+1)(true)
    isPrimeArr(0) = false
    isPrimeArr(1) = false

    (2 until isPrimeArr.size).foreach(i =>
      if (isPrimeArr(i)) {
        var j = i+i
        while (j < isPrimeArr.size) {
          isPrimeArr(j) = false
          j += i
        }
      }
    )

    (2 until isPrimeArr.size).foldLeft(Set(2))((ps, i) => if (isPrimeArr(i)) ps+i else ps)
  }


  /** Si un numero es primo o no */
  def isPrime(num: Int): Boolean = {
    if (num < 2) false
    else if (num == 2) true
    else if (num % 2 == 0) false
    else (3 to sqrt(num).toInt by 2).forall(num % _ != 0)
  }


  def allPrimes: LazyList[Int] = LazyList.from(2).filter(isPrime(_))
}
