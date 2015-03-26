package tests

import org.scalatest._
import com.c2scala.CParser
import com.c2scala.CLexer

import com.c2scala.DeclarationConverter
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;
import org.antlr.runtime.Token;
import reflect.runtime.universe._

class EnumTypedef extends FlatSpec with ShouldMatchers {

  "A simple enum typedef" should "convert correctly" in {
    val test = """typedef enum {
                    LINE_TYPE  = 0x00000001,
                    POINT_TYPE = 0x00000002
                  } OBSTACLE_TYPE;"""

    val expected = """type OBSTACLE_TYPE = Int;
                      val LINE_TYPE: OBSTACLE_TYPE = 0x00000001;
                      val POINT_TYPE: OBSTACLE_TYPE = 0x00000002;
                    """
    
    assert(test ==> expected)
  }
  
  "A simple enum typedef with implicit numbering" should "convert correctly" in {
    val test = """typedef enum {
                    LINE_TYPE  = 0,
                    POINT_TYPE = LINE_TYPE,
                    SQUARE_TYPE
                  } OBSTACLE_TYPE;"""

    val expected = """type OBSTACLE_TYPE = Int;
                      val LINE_TYPE: OBSTACLE_TYPE = 0;
                      val POINT_TYPE: OBSTACLE_TYPE = LINE_TYPE;
                      val SQUARE_TYPE: OBSTACLE_TYPE = 1;
                    """
    
    assert(test ==> expected)
  }
  
  "A complex enum typedef with implicit numbering" should "convert correctly" in {
    val name = System.nanoTime
    
    val test = """typedef enum {
                    LINE_TYPE  = 0,
                    POINT_TYPE = LINE_TYPE,
                    SQUARE_TYPE,
                    OVAL_TYPE
                  } OBSTACLE_TYPE;"""

    convertedToScala(test) should equal(Array("type OBSTACLE_TYPE = Int",
                                                                       "val LINE_TYPE: OBSTACLE_TYPE = 0",
                                                                       "val POINT_TYPE: OBSTACLE_TYPE = LINE_TYPE",
                                                                       "val SQUARE_TYPE: OBSTACLE_TYPE = 1",
                                                                       "val OVAL_TYPE: OBSTACLE_TYPE = 2"
                                                                       ))
  }
}