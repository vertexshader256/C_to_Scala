package tests

import org.scalatest._

class StructTypedefWithTypeVar extends FlatSpec with ShouldMatchers {

  "A simple struct typedef conversion with a variable named 'type'" should "convert correctly" in {
    val name = System.nanoTime
    
  val test = """typedef struct {
                  OBSTACLE_TYPE type;
                } POINT_OBSTACLE;"""
  
  val result = """class POINT_OBSTACLE {
                  var obstacle_type: OBSTACLE_TYPE = null
                }"""
    
    assert(test ==> result)
  }
}