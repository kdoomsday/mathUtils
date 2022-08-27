package math

import scala.annotation.tailrec

import scala.{ math => sm }
import sm.sqrt

import Primes.primeSieve

/**
 * Matematica general
 */
object General {

  /** Factorial de num */
  def factorial(num: BigInt): BigInt = {
    @tailrec def fh(curr: BigInt, tot: BigInt): BigInt =
      if (curr == BigInt(1)) tot
      else fh(curr - 1, tot*curr)

    fh(num, 1)
  }


  // base ^ pow
  def pow(base: BigInt, pow: BigInt): BigInt = {
    def powh(b: BigInt, p: BigInt): BigInt =
      if (p == 1) b
      else if (p % 2 == 0) powh(b*b, p / 2)
      else b * powh(b*b, p/2)

    if (pow == 0) 1
    else powh(base, pow)
  }

  // Logaritmo base b de x
  def logb (b: Double, x: Double): Double = {
    import scala.math.log
    log (x) / log (b)
  }


  // Descomposicion en factores primos de n
  def decomp(num: Int): Seq[Int] = {
    val ps = primeSieve(sqrt(num).toInt)

    @tailrec
    def dc(i: Int, divs: Seq[Int]): Seq[Int] = {
      if (i == 1) divs
      else {
        ps.take(sqrt(i).toInt).find(x => i%x == 0) match {
          case Some(x) => dc(i / x, divs :+ x)
          case None    => divs :+ i
        }
      }
    }

    dc(num, Seq())
  }


  // Descomposicion en factores primos de n (usando seq de primos)
  // Ligeramente mas rapido si ya tienes los primos
  def decomp(num: Int, primes: Seq[Int]): Seq[Int] = {

    @tailrec
    def dc(i: Int, divs: Seq[Int]): Seq[Int] = {
      if (i == 1) divs
      else {
        primes.take(sqrt(i).toInt).find(x => i%x == 0) match {
          case Some(x) => dc(i / x, divs :+ x)
          case None    => divs :+ i
        }
      }
    }

    dc(num, Seq())
  }


  // Todos los divisores de n
  def divisores(n: Int): Set[Int] = {
    val st = sqrt(n).toInt
    def ds(curr: Int, acc: Set[Int]): Set[Int] = {
      if (curr > st) acc
      else if (n%curr == 0) ds(curr+1, (acc + curr) + n/curr)
      else ds(curr+1, acc)
    }

    ds(2, Set(1, n))
  }


  // Maximo comun divisor
  def gcd(x: BigInt, y: BigInt) = {
    @tailrec
    def gcdRec(x: BigInt, y: BigInt): BigInt = {
      if(y == 0) x else gcdRec(y, x % y)
    }
    gcdRec(x, y)
  }

  def gcd(x: Int, y: Int) = {
    @tailrec
    def gcdRec(x: Int, y: Int): Int = {
      if(y == 0) x else gcdRec(y, x % y)
    }
    gcdRec(x, y)
  }



  /**
   * Todos los euler totient por debajo de top, rapido.
   */
  def phimaker(top: Int) = {
    val fact = (0 to top).toArray

    def doP(p: Long) = {
      for (i <- p.toInt to top by p.toInt) {
        fact(i) = (fact(i) * (p-1) / p).toInt
      }
    }

    def nextP(prevp: Int): Int = {
      for (i <- prevp+1 to top if (fact(i) == i)) return i
      return -1
    }

    var p = nextP(1)
    while (p != -1) {
      doP(p)
      p = nextP(p)
    }

    fact
  }


  // Raiz n de a
  def nroot(n: Int, a: Double): Double = {
    def loop(x0: Double) : Double = {
      val x1 = (1.0d/n * ((n - 1) * x0 + a/sm.pow(x0, n-1)))
      if (x0 <= x1) x0
      else loop(x1)
    }

    return loop(a/2)
  }


  /* Inverso multiplicativo de a modulo p
   * Es un numero x tal que a*x = 1 mod p
   * Pudiera no existir, en cuyo caso devuelve None
   *
   * Algoritmo sacado de https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
   */
  def inverse(a: Long, p: Long): Option[Long] = {
    var t = 0L
    var newt = 1L
    var r = p
    var newr = a

    while (newr != 0) {
      val quot = r / newr

      var tmp = newt
      newt = t - quot * newt
      t = tmp

      tmp = newr
      newr = r - quot * newr
      r = tmp
    }

    if (r > 1) None
    else if (t < 0) Some(t + p)
    else Some(t)
  }


  /**
   * Chinese Remainder
   * Consigue el numero x mas bajo tal que para cada residuo r y modulo m (emparejados en las seqs)
   * x = r mod m
   */
  def chineseRemainder(residues: Seq[Int], modulii: Seq[Int]): Option[Int] = {
    def zipf[T](s1: Seq[Int], s2: Seq[Int], f: (Int, Int) => T) =
      s1.zip(s2).map { case (a, b) => f(a,b) }

    val modPi = modulii.product
    val crtModulii = modulii.map(modPi / _)

    val inverses = zipf(crtModulii, modulii, (a, b) => inverse(a, b))
    val f1 = residues.zip(inverses).map { case (a, b) => if(b.isDefined) a*b.get else 0}
    val f2 = crtModulii.zip(f1).map { case (a, b) => a*b}.sum

    if (f2 == 0) None
    else Some(f2.toInt % modPi)
  }
}
