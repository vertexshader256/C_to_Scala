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

package object tests {
  
  def convertedToScala(text: String): Array[String] = {
      val parser = new CParser(
              new CommonTokenStream(
                      new CLexer(
                              new ANTLRInputStream(text))));
    
      parser.setBuildParseTree(true);
  
      // This line prints the error
      val ctx = parser.compilationUnit();
      val visitor = new CConverter();
      visitor.visit(ctx);
      
      visitor.results.toList.flatMap{_.split("\n").map(_.trim)}.toArray
  }
}