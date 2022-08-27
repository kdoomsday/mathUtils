import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import org.scalacheck.Gen

object TestGeneral extends Properties("General Math") {
  import math.General._

  // Pruebo con ints limitados, porque factorial(n) para n grande es leeeento
  property("factorial size") = forAll(Gen.choose(1, 100)) { (i: Int) =>
    i <= factorial(i)
  }

  property("factorial construction") = forAll(Gen.choose(1, 100)) { (i: Int) =>
    factorial(i+1) == ( factorial(i) * (i+1) )
  }
}
