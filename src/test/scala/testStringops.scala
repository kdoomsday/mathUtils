import org.scalacheck.Properties
import org.scalacheck.Prop.{ forAll, propBoolean }

import Generators.positiveGen

object TestStringOps extends Properties("StringOps") {
  import math.StringOps._

  property("digits") = forAll(positiveGen) { i =>
    (i >= 0) ==> (digits(i) == i.toString.length)
  }

  property("palindrome string") = forAll { (i: Int) =>
    isPalindrome(i.toString + i.toString.reverse)
  }

  property("palindrome int") = {
    isPalindrome(1928291) &&
      isPalindrome(192291) &&
      !isPalindrome(42) &&
      !isPalindrome(12345321) &&
      !isPalindrome(123432)
  }
}
