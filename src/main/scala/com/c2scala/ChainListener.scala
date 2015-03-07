package com.c2scala

import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ListTokenSource;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;
import org.antlr.v4.runtime.Token;
import scala.collection.mutable.ListBuffer

class ChainListener extends CBaseListener{
  
  val results = ListBuffer[String]()
  
  protected def convertTypeName(varName: String, typeName: String) = {
    if (varName == "type") {
      typeName.toLowerCase()
    } else {
      varName
    }
  }
  
  protected def translateTypeSpec(typeSpec: CParser.TypeSpecifierContext) = typeSpec.getText match {
    case "char" => "Char"
    case "float" => "Float"
    case "long" => "Long"
    case "short" => "Short"
    case "int" => "Int"
    case _ => typeSpec.getText
  }
  
  protected def getTypeDefault(typeSpecifier: String) = typeSpecifier match {
    case "char" | "long" | "short" | "int" => "0"
    case "float" | "double" => "0.0"
    case _ => "null"
  }
  
  private def copyTreeRecursive(original: ParseTree ): List[Token] = {
    if (original.getChildCount == 0) {
      List(original.getPayload.asInstanceOf[Token])
    } else {    
      (0 until original.getChildCount).toList.flatMap{x => copyTreeRecursive(original.getChild(x))}
    }
  }
  
  def translate(ctx: ParseTree) = {
    import scala.collection.JavaConverters._

    val parser = new CParser(
            new CommonTokenStream(new ListTokenSource(copyTreeRecursive(ctx).asJava)))
    
      parser.setBuildParseTree(true);

      // This line prints the error
      val compilationUnit = parser.compilationUnit();
      
      ParseTreeWalker.DEFAULT.walk(this, compilationUnit); 
        
      results
  }
}