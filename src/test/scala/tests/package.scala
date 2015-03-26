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
import scala.collection.mutable.HashMap
import scala.reflect.api.JavaUniverse
import scala.tools.reflect.ToolBox
import scala.reflect.runtime.currentMirror
package object tests {
  
  //val universe = reflect.runtime.universe;
  val toolbox = currentMirror.mkToolBox()
  
  def convertedToScala(text: String): Array[String] = {
    
  
      val parser = new CParser(
              new CommonTokenStream(
                      new CLexer(
                              new ANTLRInputStream(text))));
    
      parser.setBuildParseTree(true);
      val cTypes = HashMap[String, String]()
      // This line prints the error
      val ctx = parser.compilationUnit();
      val visitor = new DeclarationConverter(cTypes, true);
      visitor.visit(ctx);
      
      visitor.results.toList.flatMap{_.split("\n").map(_.trim)}.toArray
  }
  
  def convertedToScalaTree(text: String): toolbox.u.Tree = {   
    
      val parser = new CParser(
              new CommonTokenStream(
                      new CLexer(
                              new ANTLRInputStream(text))));
    
      parser.setBuildParseTree(true);
      val cTypes = HashMap[String, String]()
      // This line prints the error
      val ctx = parser.compilationUnit();
      val visitor = new DeclarationConverter(cTypes, true);
      visitor.visit(ctx);
      
      val result = visitor.results.toList.reduce(_ + ";" + _).mkString
      toolbox.parse(result)
  }
}