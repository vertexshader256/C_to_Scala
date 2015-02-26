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

class SimpleTypedef extends FlatSpec with ShouldMatchers {

  "A simple typedef conversion" should "convert correctly" in {
    val name = System.nanoTime
    
    val parser = new CParser(
            new CommonTokenStream(
                    new CLexer(
                            new ANTLRInputStream("typedef S32 LATLON;"))));
  
    parser.setBuildParseTree(true);

    // This line prints the error
    val ctx = parser.compilationUnit();
    val listener = new CConverter();
    ParseTreeWalker.DEFAULT.walk(listener, ctx); 

    listener.results(0).trim should equal("type LATLON = S32")
  }
}