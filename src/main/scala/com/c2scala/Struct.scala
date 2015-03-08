package com.c2scala

import scala.collection.mutable.ListBuffer

import scala.collection.mutable.HashMap
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.Token

trait Struct {
  val structDecl: ListBuffer[String]
}

class StructConverter(cTypes: HashMap[String, String]) extends ChainListener[Struct](cTypes) {
  var declarationHasStruct = false
  var declarationHasTypedefStruct = false
  val structDeclarations = ListBuffer[String]()
  
  var specifierQualifierLevel = 0
  var currentTypeSpec: CParser.TypeSpecifierContext = null

  var isTypeEnum = false
  var isWithinFunction = false
  var hasTypedefName = false

  val typedefNames = ListBuffer[String]()
  var latestStorageSpecifier = ""
  var latestTypeSpec: CParser.TypeSpecifierContext = null
  var latestDirectDeclarator = ""
  
  var latestArraySize = 0
  var isArray = false
  
  var latestStructDecName = ""
  var islatestStructDecArray = false

   override def aggregateResult(aggregate: Struct, nextResult: Struct): Struct = {
    if (aggregate == null) {
        nextResult
    } else if (nextResult == null) {
        aggregate
    } else {
      null
    }
  }
  
  override def visitSpecifierQualifierList(ctx: CParser.SpecifierQualifierListContext) = {
    specifierQualifierLevel += 1
    super.visitSpecifierQualifierList(ctx)
    specifierQualifierLevel -= 1
    null
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
    null
  }
  
  override def visitDirectDeclarator(ctx: CParser.DirectDeclaratorContext) = {
    isArray = true
    islatestStructDecArray = true
    latestDirectDeclarator = ctx.getText
    super.visitDirectDeclarator(ctx)
    null
  }
 
   
  override def visitTypedefName(ctx: CParser.TypedefNameContext) = {   
    hasTypedefName = true
    latestStructDecName = ctx.Identifier().getText
    null
  }
  
  override def visitStructDeclaration(ctx: CParser.StructDeclarationContext) = {
    latestStructDecName = ""
    islatestStructDecArray = false
    super.visitStructDeclaration(ctx)
    if (islatestStructDecArray && latestArraySize != 0) {
        structDeclarations += "var " + latestDirectDeclarator + ": Array[" + translateTypeSpec(currentTypeSpec) + "]" + " = Array.fill(" + latestArraySize + ")(" + getTypeDefault(currentTypeSpec.getText) + ")"//type " + latestDirectDeclarator + " = Array[" + typedefNames(0) + "]\n"
    } else if (islatestStructDecArray && latestArraySize == 0) {
        structDeclarations += "var " + latestDirectDeclarator + ": Array[" + translateTypeSpec(currentTypeSpec) + "]" + " = null"//type " + latestDirectDeclarator + " = Array[" + typedefNames(0) + "]\n"
    } else if (currentTypeSpec != null) {
        println(getTypeDefault(cTypes.withDefaultValue("couldnt find")(currentTypeSpec.getText)))
        val baseTypeDefault = getTypeDefault(cTypes.withDefaultValue(currentTypeSpec.getText)(currentTypeSpec.getText))
        structDeclarations += "var " + convertTypeName(latestStructDecName, currentTypeSpec.getText) + ": " + translateTypeSpec(currentTypeSpec) + " = " + baseTypeDefault
    }
    null
  }
   
  override def visitTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {
    hasTypedefName = false
    super.visitTypeSpecifier(ctx)
    if (!hasTypedefName)
      latestTypeSpec = ctx
      
      if (specifierQualifierLevel == 1) {
        currentTypeSpec = ctx
      } 
    null
  }
 
  
  override def visitStructOrUnionSpecifier(ctx: CParser.StructOrUnionSpecifierContext) = {
    declarationHasStruct = true
    structDeclarations.clear
    latestStorageSpecifier = ""
    latestTypeSpec = null
    currentTypeSpec = null
    declarationHasStruct = false
    isTypeEnum = false
    isWithinFunction = false
    isArray = false
    typedefNames.clear
    
    super.visitStructOrUnionSpecifier(ctx)
    
    new Struct { val structDecl = structDeclarations }
  }
 
  override def visitStorageClassSpecifier(ctx: CParser.StorageClassSpecifierContext) = {
    //println("ENTERING TYPEDEF: " +ctx.getText)
    latestStorageSpecifier = ctx.getText
    null
    //super.visitStorageClassSpecifier(ctx)
  }

}