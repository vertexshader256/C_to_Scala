package com.c2scala

import scala.collection.mutable.ListBuffer

import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.Token
import scala.collection.mutable.HashMap

class TypedefConverter(cTypes: HashMap[String, String]) extends ChainListener[Unit](cTypes) {
  var struct: Struct = null
  var currentTypeSpec: CParser.TypeSpecifierContext = null
  
  val typedefNames = ListBuffer[String]()
  val directDeclarators = ListBuffer[String]()
  val explicitInitValues = ListBuffer[String]()
  var latestStorageSpecifier = ""
  var latestTypeSpec: CParser.TypeSpecifierContext = null
  var latestDirectDeclarator = ""
  
  var latestArraySize = 0
  var isArray = false
  var enumeration: Enumeration = null
    
  override def visitDeclaration(ctx: CParser.DeclarationContext) = {
    latestStorageSpecifier = ""
    latestTypeSpec = null
    currentTypeSpec = null
    isArray = false
    typedefNames.clear
    
    super.visitDeclaration(ctx)
    
    if (struct != null && !typedefNames.isEmpty) {
      var result = "class " + typedefNames(0) + " {\n"
      //structDeclarations.foreach(println)
      if (!struct.structDecl.isEmpty) {
        result += struct.structDecl.map("  " + _).reduce{_ + "\n" + _}
      }
      result += "\n}"
      results += result
    } else if (isArray && typedefNames.size == 1) {
      results += "type " + latestDirectDeclarator + " = Array[" + typedefNames(0) + "]\n"
    } else if (enumeration == null) {
      if (typedefNames.size == 1) {
        results += "type " + typedefNames(0) + " = " + translateTypeSpec(latestTypeSpec) + "\n"
        cTypes += typedefNames(0) -> latestTypeSpec.getText
      } else if (typedefNames.size == 2) {
        results += "type " + typedefNames(1) + " = " + typedefNames(0) + "\n"
        cTypes += typedefNames(1) -> typedefNames(0)
      }
    } else if (enumeration != null) {
        results += "type " + enumeration.name + " = Int"
        enumeration.enumerators.foreach{enum =>
          results += ("val " + enum.name + ": " + typedefNames(0) + " = " + enum.expression)
      }
    }
  }
  
  override def visitInitDeclaratorList(ctx: CParser.InitDeclaratorListContext) = {
    directDeclarators.clear
    super.visitInitDeclaratorList(ctx)
  }
  
  override def visitInitializer(ctx: CParser.InitializerContext) = {
    explicitInitValues += ctx.getText
  }
  
  override def visitDirectDeclarator(ctx: CParser.DirectDeclaratorContext) = {
    isArray = true
    latestDirectDeclarator = ctx.getText
    directDeclarators += ctx.getText
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
    typedefNames += ctx.Identifier().getText
  }
    
  override def visitTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
    super.visitTypeSpecifier(ctx)
    if (ctx.typedefName() == null) {
      latestTypeSpec = ctx
    }
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