package tests

import org.scalatest._
import com.c2scala.CParser
import com.c2scala.CLexer

import com.c2scala.CConverter
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;
import org.antlr.runtime.Token;

class EnumTypedef extends FlatSpec with ShouldMatchers {

  "A simple enum typedef" should "convert correctly" in {
    val name = System.nanoTime
    
    val test = "typedef enum {\n" +
      "LINE_TYPE  = 0x00000001,\n" +
      "POINT_TYPE = 0x00000002\n" +
      "} OBSTACLE_TYPE;"

    val parser = new CParser(
            new CommonTokenStream(
                    new CLexer(
                            new ANTLRInputStream(test))));
  
    parser.setBuildParseTree(true);

    // This line prints the error
    val ctx = parser.compilationUnit();
    val listener = new CConverter();
    ParseTreeWalker.DEFAULT.walk(listener, ctx); 

    listener.results.map(_.trim).filter(_.size > 0) should equal(Array("type OBSTACLE_TYPE = Int",
                                                                       "val LINE_TYPE: OBSTACLE_TYPE = 0x00000001",
                                                                       "val POINT_TYPE: OBSTACLE_TYPE = 0x00000002"
                                                                       ))
  }
  
  "A simple enum typedef with implicit numbering" should "convert correctly" in {
    val name = System.nanoTime
    
    val test = "typedef enum {\n" +
      "LINE_TYPE  = 0,\n" +
      "POINT_TYPE = LINE_TYPE,\n" +
      "SQUARE_TYPE = 1\n" +
      "} OBSTACLE_TYPE;"

    val parser = new CParser(
            new CommonTokenStream(
                    new CLexer(
                            new ANTLRInputStream(test))));
  
    parser.setBuildParseTree(true);

    // This line prints the error
    val ctx = parser.compilationUnit();
    val listener = new CConverter();
    ParseTreeWalker.DEFAULT.walk(listener, ctx); 

    listener.results.map(_.trim).filter(_.size > 0) should equal(Array("type OBSTACLE_TYPE = Int",
                                                                       "val LINE_TYPE: OBSTACLE_TYPE = 0",
                                                                       "val POINT_TYPE: OBSTACLE_TYPE = LINE_TYPE",
                                                                       "val SQUARE_TYPE: OBSTACLE_TYPE = 1"
                                                                       ))
  }
}