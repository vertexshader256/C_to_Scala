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
  
  protected def translateTypeSpec(typeSpec: CParser.TypeSpecifierContext, isPointer: Boolean = false) = (typeSpec.getText, isPointer) match {
    case ("char", _) => "Char"
    case ("float", _) => "Float"
    case ("double", _) => "Double"
    case ("long", _) => "Long"
    case ("short", _) => "Short"
    case ("int", _) => "Int"
    case ("void", false) => "Unit"
    case ("void", true) => "Object"
    case _ => typeSpec.getText
  }
  
  protected def getTypeDefault(typeSpecifier: String) = typeSpecifier match {
    case "char" | "long" | "short" | "int" | "Char" | "Long" | "Short" | "Int"=> "0"
    case "float" | "double" | "Float" | "Double"=> "0.0"
    case _ => "null"
  }
}