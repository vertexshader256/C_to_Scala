package tests

import org.scalatest._

class ArrayTypedefInt extends FlatSpec with ShouldMatchers {

  "An array typedef conversion" should "convert correctly" in {

    assert("typedef U32 OBSTACLE_LIST[(2048)];" ==> "type OBSTACLE_LIST = Array[U32]")
  }
}