package com.c2scala

import org.antlr.v4.runtime.CommonTokenStream;
import scala.collection.mutable.HashMap
import org.antlr.v4.runtime.ListTokenSource;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.TreeAdaptor;
import org.antlr.v4.runtime.Token;
import scala.collection.mutable.ListBuffer

class ChainListener[X](val cTypes: HashMap[String, String]) extends CBaseVisitor[X] {
  
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
}