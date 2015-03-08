package tests

import org.scalatest._

class ExternVar extends FlatSpec with ShouldMatchers {

  "A simple typedef conversion" should "convert correctly" in {

    convertedToScala("extern int ta_cm_upd;").isEmpty should equal(true)
  }
}