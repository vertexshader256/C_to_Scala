package com.c2scala

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.util.Try

/*******************************************************
 * declaration
    :   declarationSpecifiers initDeclaratorList? ';'
    |   staticAssertDeclaration
    ;
    
    staticAssertDeclaration not yet supported
    
 *********************************************************/

class DeclarationConverter(cTypes: HashMap[String, String], outputFunctionContents: Boolean) extends ChainListener[Unit](cTypes) {

  val typedefNames = ListBuffer[String]()
  var typeQualifier = ""
  var latestStorageSpecifier = ""
  var typeName = ""
  var varName = ""
  var islatestStructDecArray = false
  var latestPrimaryExpression = ""
  var currentTypeSpec: CParser.TypeSpecifierContext = null
  var specifierQualifierLevel = 0
  
  def isTypedef(ctx: CParser.DeclarationContext): Boolean = {
    val checkTypedef = Try(ctx.declarationSpecifiers().declarationSpecifier().get(0).getText == "typedef")
    checkTypedef.getOrElse(false)
  }
  
  override def visitDeclaration(ctx: CParser.DeclarationContext) = {
    latestStorageSpecifier = ""
    typeQualifier = ""
    typedefNames.clear
      
    if (!isTypedef(ctx)) {
      super.visitDeclaration(ctx)
      // when there is a single variable with no initializer e.g "int blah;"
      if (ctx.initDeclaratorList == null) {
        if ((latestStorageSpecifier == "" || latestStorageSpecifier == "static")) {       
          val scope = if (latestStorageSpecifier == "static") "private" else ""
          val qualifier = scope + " " + (if (typeQualifier == "const") "val" else "var")
          val baseTypeDefault = getDefault(cTypes, typeName)
          results += qualifier + " " + varName + ": " + typeName + " = " + postProcessValue(baseTypeDefault, typeName) + "\n"
        } 
      }
    } else {
      val typedefConverter = new TypedefConverter(cTypes)
      typedefConverter.visit(ctx)
      results ++= typedefConverter.results
    }
  }
  
  def parseSimpleDecl() = {
    results += "var " + convertTypeName(varName, typeName) + ": " +
    (if (islatestStructDecArray && latestPrimaryExpression != "") {
        "Array[" + translateTypeSpec(currentTypeSpec) + "]" + " = Array.fill(" + latestPrimaryExpression + ")(" + getDefault(cTypes, currentTypeSpec.getText) + ")"
    } else if (islatestStructDecArray) {
        "Array[" + translateTypeSpec(currentTypeSpec) + "]" + " = null"
    } else if (currentTypeSpec != null) {
        val baseTypeDefault = postProcessValue(getDefault(cTypes, typeName), typeName)
        typeName + " = " + baseTypeDefault
    })
  }
  
  override def visitSpecifierQualifierList(ctx: CParser.SpecifierQualifierListContext) = {
    specifierQualifierLevel += 1
    super.visitSpecifierQualifierList(ctx)
    specifierQualifierLevel -= 1
  }
  
  override def visitPrimaryExpression(ctx: CParser.PrimaryExpressionContext) = {
    super.visitPrimaryExpression(ctx)
    if (ctx.expression() == null) { // is this the bottom of the tree?!
      latestPrimaryExpression = if (ctx.getText.contains("0x")) {
        Integer.getInteger(ctx.getText.drop(2), 16).toString
      } else {
        ctx.getText
      }
    }
  }
  
  override def visitStructDeclaration(ctx: CParser.StructDeclarationContext) = {
    super.visitStructDeclaration(ctx)

    (if (!islatestStructDecArray && currentTypeSpec != null) {
        val baseTypeDefault = postProcessValue(getDefault(cTypes, typeName), typeName)
        results += "var " + convertTypeName(varName, typeName) + ": " + typeName + " = " + baseTypeDefault
    })
  }
  
  override def visitDeclarator(ctx: CParser.DeclaratorContext) = {
    super.visitDeclarator(ctx)
    parseSimpleDecl()
     val scope = if (latestStorageSpecifier == "static") "private" else ""
    val qualifier = scope + " " + (if (typeQualifier == "const") "val" else "var")

    val contents = new DeclaratorConverter(cTypes, if (!typedefNames.isEmpty) typedefNames(0) else typeName, latestStorageSpecifier, qualifier)
      contents.visit(ctx)
      results ++= contents.results
  }
  
  override def visitTypeQualifier(ctx: CParser.TypeQualifierContext) = {
    typeQualifier =  ctx.getText
  }
  
  override def visitInitDeclaratorList(ctx: CParser.InitDeclaratorListContext) = {
    val scope = if (latestStorageSpecifier == "static") "private" else ""
    val qualifier = scope + " " + (if (typeQualifier == "const") "val" else "var")

    val contents = new DeclaratorConverter(cTypes, if (!typedefNames.isEmpty) typedefNames(0) else typeName, latestStorageSpecifier, qualifier)
      contents.visit(ctx)
      results ++= contents.results
  }
  
  override def visitDirectDeclarator(ctx: CParser.DirectDeclaratorContext) = {
    islatestStructDecArray = true
    varName = ctx.getText
    super.visitDirectDeclarator(ctx)
  }
     
  override def visitTypedefName(ctx: CParser.TypedefNameContext) = {
    if (typedefNames.size == 1) {
     typeName = typedefNames(0)
    } else {
      typedefNames += ctx.Identifier().getText
    }
    varName = ctx.Identifier().getText
  }
    
  override def visitTypeSpecifier(ctx: CParser.TypeSpecifierContext) = {    
    if (ctx.typedefName() == null) {      
      typeName = translateTypeSpec(ctx)
    } else { 
      super.visitTypeSpecifier(ctx)
    }
    currentTypeSpec = ctx
  }
  
  override def visitFunctionDefinition(ctx: CParser.FunctionDefinitionContext) = {
    results ++= new FunctionConverter(cTypes, outputFunctionContents).visit(ctx)
  }
 
  override def visitStorageClassSpecifier(ctx: CParser.StorageClassSpecifierContext) = {
    latestStorageSpecifier = ctx.getText
  }

}