import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object PrimeSpec extends Properties("Prime") {
  import math.Primes._

  val firstPrimes = List(2, 3, 5, 7, 11, 13, 17, 19)

  property("allPrimes") = {
    allPrimes.zip(firstPrimes).forall { case (x, y) => x == y }
  }

  property("primeSieve") = {
    primeSieve(20).zip(firstPrimes).forall { case (x, y) => x == y }
  }

  property("allPrimes vs primeSieve") = {
    primeSieve(1000).zip(allPrimes).forall { case (x, y) => x == y }
  }

  property("isPrime") = {
    isPrime(6659) && isPrime(9851) && !isPrime(98512) && !isPrime(9853)
  }
}
