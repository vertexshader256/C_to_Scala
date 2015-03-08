package tests

import org.scalatest._

class Statement extends FlatSpec with ShouldMatchers {

  "A simple statement" should "convert correctly" in {
    convertedToScala("int blah;").head should equal("var blah: Int = 0")
  }
  
  "A simple statement with custom type" should "convert correctly" in {
    convertedToScala("LATLON blah;").head should equal("var blah: LATLON = null")
  }
}