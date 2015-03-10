package com.c2scala

import scala.collection.mutable.ListBuffer

import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.Token
import scala.collection.mutable.HashMap

case class Typedef(name: String, expr: String)

class TypedefConverter(cTypes: HashMap[String, String]) extends ChainListener[Unit](cTypes) {
  var struct: Struct = null
  var currentTypeSpec: CParser.TypeSpecifierContext = null
  
  var latestTypedefName = ""
  val explicitInitValues = ListBuffer[String]()
  var latestStorageSpecifier = ""
  var latestTypeSpec: CParser.TypeSpecifierContext = null
  
  var typedef: Typedef = null
  
  var latestArraySize = 0
  var isArray = false
  var enumeration: Enumeration = null
    
  override def visitDeclaration(ctx: CParser.DeclarationContext) = {
    latestStorageSpecifier = ""
    latestTypeSpec = null
    currentTypeSpec = null
    isArray = false
    latestTypedefName = ""
    
    super.visitDeclaration(ctx)
    
    if (struct != null) {
      var result = "class " + struct.name + " {\n"
      if (!struct.structDecl.isEmpty) {
        result += struct.structDecl.map("  " + _).reduce{_ + "\n" + _}
      }
      result += "\n}"
      results += result
    } else if (enumeration != null) {
        results += "type " + enumeration.name + " = Int"
        enumeration.enumerators.foreach{enum =>
          results += ("val " + enum.name + ": " + enumeration.name + " = " + enum.expression)
      }
    } else if (typedef != null) {
      results += "type " + typedef.name + " = " + typedef.expr + "\n"
      cTypes += typedef.name -> typedef.expr
    } 
  }
  
  override def visitInitializer(ctx: CParser.InitializerContext) = {
    explicitInitValues += ctx.getText
  }
  
  override def visitDirectDeclarator(ctx: CParser.DirectDeclaratorContext) = {
    isArray = true
    typedef = Typedef(ctx.getText, "Array[" + latestTypedefName + "]")
    super.visitDirectDeclarator(ctx)
    
  }
  
  override def visitPrimaryExpression(ctx: CParser.PrimaryExpressionContext) = {
    super.visitPrimaryExpression(ctx)
    if (ctx.expression() == null) { // is this the bottom of the tree?!
      latestArraySize = if (ctx.getText.contains("0x")) {
        Integer.getInteger(ctx.getText.drop(2), 16)
      } else if (ctx.getText forall Character.isDigit) {
          ctx.getText.toInt
      } else {
        0
      }
    }
  }
   
  override def visitTypedefName(ctx: CParser.TypedefNameContext) = { 
    if (!isArray) {
      if (latestTypedefName == "" && latestTypeSpec != null) {
        typedef = Typedef(ctx.Identifier().getText, translateTypeSpec(latestTypeSpec))
      } else if (latestTypedefName != "") {
        typedef = Typedef(ctx.Identifier().getText, latestTypedefName)
      }
    }
    latestTypedefName = ctx.Identifier().getText
  }
    
  override def visitTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
    super.visitTypeSpecifier(ctx)
    latestTypeSpec = ctx
  }
    
  override def visitEnumSpecifier(ctx: CParser.EnumSpecifierContext) = {
    enumeration = new EnumConverter(cTypes).visitEnumSpecifier(ctx)
  }
  
  override def visitFunctionDefinition(ctx: CParser.FunctionDefinitionContext) = {
    results ++= new FunctionConverter(cTypes).visitFunctionDefinition(ctx)
    super.visitFunctionDefinition(ctx)
  }
  
  override def visitStructOrUnionSpecifier(ctx: CParser.StructOrUnionSpecifierContext) = {
    struct = new StructConverter(cTypes).visitStructOrUnionSpecifier(ctx)
  }
 
  override def visitStorageClassSpecifier(ctx: CParser.StorageClassSpecifierContext) = {
    latestStorageSpecifier = ctx.getText
  }
}