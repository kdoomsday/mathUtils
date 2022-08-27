import org.scalacheck.Gen._
import org.scalacheck.Arbitrary.arbitrary

object Generators {
  // Generador de numeros positivos
  val positiveGen = for (i <- arbitrary[Int]) yield if (i >= 0) i else -i
}
