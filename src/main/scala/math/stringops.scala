package math

import scala.annotation.tailrec

/**
 * Operaciones a las que les importa la representacion de un numero, mas que el
 * numero en si
 */
object StringOps {
  /* *** Funciones que le importan los digitos de un numero *** */
  def isPalindrome(str: String): Boolean = str == str.reverse
  def isPalindrome(i: Int): Boolean = isPalindrome(i.toString)

  /** Cuantos digitos tiene un numero */
  def digits(num: Long): Int = {
    @tailrec def ds(i: Long, prevds: Int): Int = if (i < 10L) prevds
                                            else (ds(i / 10L, prevds+1))
    ds(num, 1)
  }

  def digits(num: BigInt): Int = num.toString.length


  /** Invertir rapidamente un numero (504 se vuelve 405) */
  def reverse(num: Int): Int = {
    var n = num
    var m = 0
    while(n > 0) {
        m *= 10
        m += (n % 10)
        n /= 10
    }
    m
  }
}
