package tests

import org.scalatest._

class StructTypedef extends FlatSpec with ShouldMatchers {

  "Unresolved custom typed struct members" should "be assigned to 'null'" in {
    val test = """typedef struct {
                    LATLON lat ;
                    LATLON lon ;
                  } LL;"""
    
    val result = """class LL {
                      var lat: LATLON = null
                      var lon: LATLON = null
                    }"""
    
    assert(test ==> result)
  }
  
  "Primitive typed struct members" should "be assigned to a default value" in {
    val test = """typedef struct {
                    int lat ;
                    float lon ;
                  } LL;"""
    
    
    val result = """class LL {
                      var lat: Int = 0
                      var lon: Float = 0.0f
                    }"""
    
    assert(test ==> result)
  }
  
  "A typedef with referenced type default" should "convert correctly" in {
    val test = """typedef int x;
                  typedef struct { 
                    x lat ;
                    float lon ;
                  } LL;"""
    
    val result = """type x = Int
                    class LL {
                      var lat: x = 0
                      var lon: Float = 0.0f
                    }"""
    
    assert(test ==> result)
  }
  
  "Resolved custom typed struct members" should "be assigned a default value" in {
    val test = """typedef struct {
                    LATLON lat[2048] ;
                    LATLON lon ;
                  } LL;"""
    
    val result = """class LL {
                      var lat: Array[LATLON] = Array.fill(2048)(null)
                      var lon: LATLON = null
                    }"""
    
    assert(test ==> result)
  }
  
  "Struct members which are a custom type that resolves to 'Int'" should "be initially allocated to 0, even in an array" in {
    val test = """typedef Int Blah;
                  typedef struct {
                    Blah lat[2048];
                    Blah lon;
                  } LL;"""
    
    val result = """type Blah = Int;
                    class LL {
                      var lat: Array[Blah] = Array.fill(2048)(0);
                      var lon: Blah = 0;
                    }"""
    
    assert(test ==> result)
  }
  
  "Chained custom types" should "resolve before a default value is set" in {
    val test = """typedef double Blah;
                  typedef Blah Test;
                  typedef struct {
                    Test lat[2048];
                    Test lon;
                  } LL;"""
    
    val result = """type Blah = Double;
                    type Test = Blah;
                    class LL {
                      var lat: Array[Test] = Array.fill(2048)(0.0);
                      var lon: Test = 0.0;
                    }"""
    
    assert(test ==> result)
  }
  
  "If, for some reason, there are parenthesis around the array size, it" should "not matter" in {
    val test = """typedef struct {
                    LATLON lat[((2048))] ;
                    LATLON lon ;
                  } LL;"""
    
    val result = """class LL { 
           var lat: Array[LATLON] = Array.fill(2048)(null)
           var lon: LATLON = null
         }"""

     assert(test ==> result)
  }

  "Struct members which are a pointer" should "be set to be an unitialized array" in {
    val test = """typedef struct {
                    LATLON lat;
                    LATLON *lon;
                  } LL;"""
    
    val result = """class LL { 
           var lat: LATLON = null
           var lon: Array[LATLON] = null
         }"""

    assert(test ==> result)
  }
}