package edu.jingw.algebra

import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks

class PrimeFieldSpec extends PropSpec with PropertyChecks {
  property("Multiplying by the inverse should give one") {
    forAll { (x: Int) =>
      val elem = F13(x)
      whenever(elem != F13(0)) {
        elem * elem.inverse === F13(1)
      }
    }
  }
}
